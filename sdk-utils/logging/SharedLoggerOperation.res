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
  (value) => value != null && typeof value.then === "function"
`)

external asPromise: 'value => promise<'result> = "%identity"
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
  let emit = (step, ~details=[]) =>
    onStep(
      step,
      timedOut.contents
        ? details->Array.concat([("settled_after_timeout", true->JSON.Encode.bool)])
        : details,
    )

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
    | error => Some(error->summarizeExn)
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

  try {
    let value = call()
    if value->isThenable {
      emit({outcome: Started, durationMs: None, failureClass: None, error: None, timeoutMs: 0})
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
      value
      ->asPromise
      ->Promise.then(result => {
        if tracker->settle || timedOut.contents {
          result->finish(Done)
        }
        Promise.resolve()
      })
      ->Promise.catch(error => {
        if tracker->settle || timedOut.contents {
          let error = error->summarizeExn
          failed(error->SharedLoggerFormatting.isAborted ? Aborted : Rejected, error)
        }
        Promise.resolve()
      })
      ->ignore
    } else if tracker->settle {
      value->asResult->finish(syncOutcome)
    }
    value
  } catch {
  | error => {
      if tracker->settle {
        failed(Threw, error->summarizeExn)
      }
      rethrow(error)
    }
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
