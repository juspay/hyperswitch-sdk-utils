open SharedLoggerTypes

type source =
  | HyperLoader
  | PreMountLoader
  | Headless
  | AuthenticationSession
  | Elements(string)
  | Fullscreen(string)

let sourceName = source =>
  switch source {
  | HyperLoader => "HYPER_LOADER"
  | PreMountLoader => "PRE_MOUNT_LOADER"
  | Headless => "HEADLESS"
  | AuthenticationSession => "AUTHENTICATION_SESSION"
  | Elements(mode) => `ELEMENTS_${mode->SharedLoggerEvent.screamingSnakeCase}`
  | Fullscreen(overlay) => `FULLSCREEN_${overlay->SharedLoggerEvent.screamingSnakeCase}`
  }

type resource = Script | Stylesheet

type platform = {
  emit: (
    ~category: category,
    ~spec: eventSpec,
    ~severity: severity,
    ~data: details=?,
    ~details: details=?,
    ~verbatim: details=?,
    ~exn: exn=?,
    ~failure: JSON.t=?,
    ~durationMs: float=?,
    ~paymentMethod: SharedLoggerPaymentMethod.paymentMethod=?,
    ~context: context=?,
    ~source: source=?,
    ~message: string=?,
    ~once: bool=?,
  ) => unit,
  resource: (
    ~spec: eventSpec,
    ~severity: operationSeverity,
    ~url: string,
    ~resource: resource,
    ~attributes: array<(string, string)>=?,
    ~matchQuery: bool=?,
    ~inBody: bool=?,
    ~paymentMethod: SharedLoggerPaymentMethod.paymentMethod=?,
    ~context: context=?,
    ~source: source=?,
    ~abandoned: unit => bool=?,
    ~message: string=?,
    ~onLoad: unit => unit=?,
    ~onError: exn => unit=?,
  ) => unit,
  isEnabled: severity => bool,
  shouldLogMerchantConfig: unit => bool,
  getContext: unit => context,
  setSession: (~sessionId: string=?, ~merchantId: string=?, unit) => unit,
  summarizeExn: exn => errorSummary,
  rethrow: exn => unit,
  defaultTimeoutMs: int,
  summarizeUnknown: JSON.t => errorSummary,
}

let current: ref<option<platform>> = ref(None)
let configure = platform => current := Some(platform)
let platform = () => current.contents->Option.getExn
let eventDetails = event => SharedLoggerFormatting.eventDetails(event, ~maxTextLength=256)
let shouldLogMerchantConfig = () => platform().shouldLogMerchantConfig()

let truncate = text => text->SharedLoggerFormatting.truncateTo(256)
let summarizeErrorResponse = value =>
  value
  ->SharedLoggerFormatting.summarizeErrorResponse
  ->Option.map(summary => {...summary, message: summary.message->Option.map(truncate)})

// Backend error type and code of failed replies, read by `fetchApi`. The message and the rest of
// the body are never kept.
let backendErrors: WeakMap.t<Fetch.Response.t, details> = WeakMap.make()

let backendErrorDetails = (summary: errorSummary) =>
  [("backend_error_type", summary.name->JSON.Encode.string)]->Array.concat(summary.details)

let readBackendError = response =>
  try {
    response
    ->Fetch.Response.clone
    ->Fetch.Response.json
    ->Promise.thenResolve(json =>
      json
      ->SharedLoggerFormatting.summarizeErrorResponse
      ->Option.forEach(summary =>
        backendErrors->WeakMap.set(response, summary->backendErrorDetails)->ignore
      )
    )
    ->Promise.catch(_ => Promise.resolve())
  } catch {
  | _ => Promise.resolve()
  }

// Successful replies whose body is not JSON, with the parser's error name (never its message,
// which can quote the body). Read by `fetchApi` when `checkBody` is set.
let unreadableBodies: WeakMap.t<Fetch.Response.t, string> = WeakMap.make()

let readSuccessBody = response =>
  try {
    response
    ->Fetch.Response.clone
    ->Fetch.Response.json
    ->Promise.thenResolve(_ => ())
    ->Promise.catch(error => {
      let name = try platform().summarizeExn(error).name catch {
      | _ => "UNKNOWN_ERROR"
      }
      unreadableBodies->WeakMap.set(response, name)->ignore
      Promise.resolve()
    })
  } catch {
  | _ => Promise.resolve()
  }

// `fetch` for observed API calls. Before the reply is handed on, a copy of its body is read, so
// the caller still reads the body and the API row reports what was in it:
// - a failed reply: its backend error type and code;
// - a successful reply, when `checkBody` is set: whether the body is readable. An unreadable one
//   makes the row a failure instead of `done`. Off by default, since it parses the body twice.
let fetchApi = (~checkBody=false, url, request) =>
  Fetch.fetch(url, request)->Promise.then(response =>
    if response->Fetch.Response.status === 0 {
      Promise.resolve(response)
    } else if !(response->Fetch.Response.ok) {
      response->readBackendError->Promise.thenResolve(() => response)
    } else if checkBody {
      response->readSuccessBody->Promise.thenResolve(() => response)
    } else {
      Promise.resolve(response)
    }
  )

let httpError = response => {
  let status = response->Fetch.Response.status
  {
    name: "HTTP_ERROR",
    message: Some(status->Int.toString),
    details: try backendErrors->WeakMap.get(response)->Option.getOr([]) catch {
    | _ => []
    },
  }
}

let unreadableError = parseError => {
  name: "UNREADABLE_RESPONSE",
  message: None,
  details: [("parse_error", parseError->JSON.Encode.string)],
}

let httpDetails = response => [("status_code", response->Fetch.Response.status->JSON.Encode.int)]
let httpFailure = response =>
  if !(response->Fetch.Response.ok) {
    Some(response->httpError)
  } else {
    switch try unreadableBodies->WeakMap.get(response) catch {
    | _ => None
    } {
    | Some(parseError) => Some(parseError->unreadableError)
    | None => None
    }
  }
external anyToJson: 'value => JSON.t = "%identity"

let emit = (
  ~category,
  ~spec,
  ~severity,
  ~data=?,
  ~details=?,
  ~verbatim=?,
  ~exn=?,
  ~failure=?,
  ~durationMs=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~message=?,
  ~once=?,
) =>
  platform().emit(
    ~category,
    ~spec,
    ~severity,
    ~data?,
    ~details?,
    ~verbatim?,
    ~exn?,
    ~failure=?failure->Option.map(anyToJson),
    ~durationMs?,
    ~paymentMethod?,
    ~context?,
    ~source?,
    ~message?,
    ~once?,
  )

let isEnabled = severity => platform().isEnabled(severity)
let setSession = (~sessionId=?, ~merchantId=?, ()) =>
  platform().setSession(~sessionId?, ~merchantId?, ())

let step = (~outcome, ~durationMs=?, ~failureClass=?, ~error=?, ~timeoutMs=0) => {
  outcome,
  durationMs,
  failureClass,
  error,
  timeoutMs,
}

let emitStep = (
  ~category,
  ~spec,
  ~severity: operationSeverity,
  ~data=?,
  ~details=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~message=?,
  step: step,
) => {
  let severity = switch step.outcome {
  | Started => severity.start
  | Done | Returned | Triggered => severity.success
  | Reused => severity.reused
  | Failed | TimedOut =>
    step.error->Option.mapOr(false, SharedLoggerFormatting.isAborted)
      ? severity.aborted
      : severity.failure
  }
  let outcomeDetails = []
  step.durationMs->Option.forEach(ms =>
    outcomeDetails->Array.push(("duration_ms", ms->JSON.Encode.float))
  )
  step.failureClass->Option.forEach(class =>
    outcomeDetails->Array.push(("failure_class", (class :> string)->JSON.Encode.string))
  )
  step.error->Option.forEach(error =>
    outcomeDetails->Array.pushMany(error->SharedLoggerFormatting.errorDetails)
  )
  if step.timeoutMs > 0 {
    outcomeDetails->Array.push(("timeout_ms", step.timeoutMs->JSON.Encode.int))
  }
  emit(
    ~category,
    ~spec={...spec, outcome: Some(step.outcome)},
    ~severity,
    ~data=data->Option.getOr([])->Array.concat(outcomeDetails),
    ~details?,
    ~durationMs=?step.durationMs,
    ~paymentMethod?,
    ~context?,
    ~source?,
    ~message?,
  )
}

let emitPhase = (
  ~category,
  ~spec,
  ~severity,
  ~outcome,
  ~data=?,
  ~details=?,
  ~startedAt=?,
  ~exn=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~message=?,
) => {
  let durationMs = startedAt->Option.mapOr(0., start => Date.now() -. start)
  switch outcome {
  | Started => step(~outcome)
  | Failed =>
    step(
      ~outcome,
      ~durationMs,
      ~failureClass=exn->Option.isSome ? Threw : ReturnedFailure,
      ~error=?exn->Option.map(error => platform().summarizeUnknown(error->anyToJson)),
    )
  | outcome => step(~outcome, ~durationMs)
  }->emitStep(
    ~category,
    ~spec,
    ~severity,
    ~data?,
    ~details?,
    ~paymentMethod?,
    ~context?,
    ~source?,
    ~message?,
  )
}

let observe = (
  ~category,
  ~spec,
  ~severity,
  ~data=?,
  ~details=?,
  ~timeoutMs=?,
  ~failureOf=?,
  ~detailsOf=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~syncOutcome=Returned,
  ~message=?,
  ~call,
) =>
  // A logger that cannot set up must still let the call run.
  switch try {
    let runtime = platform()
    let context = context->Option.getOr(runtime.getContext())
    let base = SharedLoggerFormatting.mergeDetails(
      ~data=data->Option.getOr([]),
      ~details=details->Option.getOr([]),
    )
    Some((runtime, context, base))
  } catch {
  | _ => None
  } {
  | None => call()
  | Some((runtime, context, base)) =>
  SharedLoggerOperation.observe(
    ~timeoutMs=timeoutMs->Option.getOr(runtime.defaultTimeoutMs),
    ~summarizeExn=runtime.summarizeExn,
    ~rethrow=error => {
      runtime.rethrow(error)
      raise(error)
    },
    ~failureOf?,
    ~detailsOf?,
    ~syncOutcome,
    ~onStep=(step, outcomeDetails) =>
      step->emitStep(
        ~category,
        ~spec,
        ~severity,
        ~data=base,
        ~details=outcomeDetails,
        ~paymentMethod?,
        ~context,
        ~source?,
        ~message?,
      ),
    ~call,
  )
  }

let observeCallback = (
  ~category,
  ~spec,
  ~severity,
  ~data=?,
  ~details=?,
  ~timeoutMs=?,
  ~failureOf=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~message=?,
  ~callback,
) =>
  callback->SharedLoggerOperation.forwardInvocation(invoke =>
    observe(
      ~category,
      ~spec,
      ~severity,
      ~data?,
      ~details?,
      ~timeoutMs?,
      ~failureOf?,
      ~paymentMethod?,
      ~context?,
      ~source?,
      ~syncOutcome=Triggered,
      ~message?,
      ~call=invoke,
    )
  )

let observeResource = (
  ~spec,
  ~severity,
  ~url,
  ~resource,
  ~attributes=?,
  ~matchQuery=?,
  ~inBody=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~abandoned=?,
  ~message=?,
  ~onLoad=?,
  ~onError=?,
) =>
  platform().resource(
    ~spec,
    ~severity,
    ~url,
    ~resource,
    ~attributes?,
    ~matchQuery?,
    ~inBody?,
    ~paymentMethod?,
    ~context?,
    ~source?,
    ~abandoned?,
    ~message?,
    ~onLoad?,
    ~onError?,
  )
