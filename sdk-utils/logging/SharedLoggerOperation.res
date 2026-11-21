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

external asResult: 'value => 'result = "%identity"
external asNullable: 'value => Nullable.t<'value> = "%identity"
external throwValue: 'value => 'result = "%raise"
@get external thenProperty: 'value => 'then = "then"
@send external onSettled: (promise<'result>, 'result => unit, 'reason => unit) => promise<unit> = "then"

let isThenable = value =>
  try {
    value->asNullable->Nullable.toOption->Option.mapOr(false, value =>
      typeof(value->thenProperty) === #function
    )
  } catch {
  | _ => false
  }

let observeThenable = (value, onDone, onFailure) =>
  try {
    value
    ->asResult
    ->Promise.resolve
    ->onSettled(onDone, reason => onFailure(reason->Js.Exn.anyToExnInternal))
    ->Promise.catch(_ => Promise.resolve())
    ->ignore
  } catch {
  | _ => ()
  }

let rethrow = error =>
  switch error {
  | Js.Exn.Error(jsError) => throwValue(jsError)
  | error => raise(error)
  }

let observe = (
  ~timeoutMs,
  ~summarizeExn: exn => errorSummary,
  ~failureOf: option<'result => option<errorSummary>>=?,
  ~dataOf: option<'result => details>=?,
  ~syncOutcome=Returned,
  ~onStep: (step, details, details) => unit,
  ~call: unit => 'value,
): 'value => {
  let tracker = makeTracker()
  let timedOut = ref(false)
  let emit = (step, ~data=[]) =>
    onStep(
      step,
      data,
      timedOut.contents
        ? [("settled_after_timeout", true->JSON.Encode.bool)]
        : [],
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
        ~data=try dataOf->Option.mapOr([], dataOf => dataOf(result)) catch {
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
      value->observeThenable(result => {
        if tracker->settle || timedOut.contents {
          result->finish(Done)
        }
      }, error => {
        if tracker->settle || timedOut.contents {
          let error = error->summarizeExn
          failed(error->SharedLoggerFormatting.isAborted ? Aborted : Rejected, error)
        }
      })
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
