open SharedLoggerTypes

type tracker = {
  startedAt: float,
  mutable settled: bool,
  mutable timer: option<timeoutId>,
}

let makeTracker = () => {startedAt: Date.now(), settled: false, timer: None}

let elapsed = tracker => Date.now() -. tracker.startedAt

let settle = tracker =>
  if tracker.settled {
    false
  } else {
    tracker.settled = true
    tracker.timer->Option.forEach(clearTimeout)
    tracker.timer = None
    true
  }

let isThenable: 'value => bool = %raw(`
  (value) => {
    try { return value != null && typeof value.then === "function"; }
    catch (_) { return false; }
  }
`)

let observeThenable: ('value, ('result => unit), (exn => unit)) => unit = %raw(`
  (value, onDone, onFailure) => {
    try {
      Promise.resolve(value).then(onDone, onFailure).catch(() => {});
    } catch (_) {}
  }
`)
external asResult: 'value => 'result = "%identity"

let observe = (
  ~timeoutMs,
  ~summarizeExn: exn => errorSummary,
  ~rethrow: exn => 'value,
  ~failureOf: option<'result => option<errorSummary>>=?,
  ~detailsOf: option<'result => details>=?,
  ~syncOutcome=Returned,
  ~onStep: (step, details) => unit,
  ~call: unit => 'value,
): 'value => {
  let tracker = makeTracker()
  let timedOut = ref(false)
  // Logging must never change what the call returns or throws, so every logging step is guarded.
  let guard = action =>
    try action() catch {
    | _ => ()
    }
  let emit = (step, ~details=[]) =>
    guard(() =>
      onStep(
        step,
        timedOut.contents
          ? details->Array.concat([("settled_after_timeout", true->JSON.Encode.bool)])
          : details,
      )
    )
  let summarize = error =>
    try error->summarizeExn catch {
    | _ => {name: "UNKNOWN_ERROR", message: None, details: []}
    }

  let failed = (failureClass, error) =>
    emit({
      outcome: Failed,
      durationMs: Some(tracker->elapsed),
      failureClass: Some(failureClass),
      error: Some(error),
      timeoutMs: 0,
    })

  let finish = (result, outcome) =>
    switch try failureOf->Option.flatMap(failureOf => failureOf(result)) catch {
    | error => Some(error->summarize)
    } {
    | Some(error) => failed(ReturnedFailure, error)
    | None =>
      emit(
        {outcome, durationMs: Some(tracker->elapsed), failureClass: None, error: None, timeoutMs: 0},
        ~details=try detailsOf->Option.mapOr([], detailsOf => detailsOf(result)) catch {
        | _ => []
        },
      )
    }

  switch try Ok(call()) catch {
  | error => Error(error)
  } {
  | Error(error) => {
      if tracker->settle {
        failed(Threw, error->summarize)
      }
      rethrow(error)
    }
  | Ok(value) =>
    guard(() =>
      if value->isThenable {
        emit({outcome: Started, durationMs: None, failureClass: None, error: None, timeoutMs: 0})
        // A timeout of 0 or less means the call is never reported as timed out.
        if timeoutMs > 0 {
          tracker.timer = Some(setTimeout(() =>
              if tracker->settle {
                timedOut := true
                emit({
                  outcome: TimedOut,
                  durationMs: Some(tracker->elapsed),
                  failureClass: None,
                  error: None,
                  timeoutMs,
                })
              }
            , timeoutMs))
        }
        value->observeThenable(result =>
          guard(() =>
            if tracker->settle || timedOut.contents {
              result->finish(Done)
            }
          )
        , error =>
          guard(() =>
            if tracker->settle || timedOut.contents {
              let error = error->summarize
              failed(error->SharedLoggerFormatting.isAborted ? Aborted : Rejected, error)
            }
          )
        )
      } else if tracker->settle {
        value->asResult->finish(syncOutcome)
      }
    )
    value
  }
}

let forwardInvocation: ('fn, (unit => 'value) => 'value) => 'fn = %raw(`
  function (fn, run) {
    if (typeof fn !== "function") {
      return fn;
    }
    var wrapped = function () {
      var self = this;
      var args = arguments;
      return run(function () {
        return fn.apply(self, args);
      });
    };
    try {
      Object.defineProperty(wrapped, "length", { value: fn.length });
    } catch (_) {}
    return wrapped;
  }
`)
