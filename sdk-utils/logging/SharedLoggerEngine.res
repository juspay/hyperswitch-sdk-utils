open! SharedLoggerTypes
module type Config = {
  let schemaVersion: int
  let defaultTimeoutMs: int
  let maxRowTextLength: int
}
module type Utils = {
  let eventName: (
    ~category: SharedLoggerTypes.category,
    ~action: SharedLoggerTypes.action,
    ~subject: string,
    ~outcome: option<SharedLoggerTypes.outcome>,
  ) => string
  let truncateTo: (string, int) => string
  let truncate: string => string
  let redact: string => string
  let safeRun: (unit => unit) => unit
  let normalizeDetails: SharedLoggerTypes.details => SharedLoggerTypes.details
  let mergeDetails: (
    ~data: SharedLoggerTypes.details,
    ~details: SharedLoggerTypes.details,
  ) => SharedLoggerTypes.details
  let fitToBudget: SharedLoggerTypes.details => SharedLoggerTypes.details
  let errorDetails: SharedLoggerTypes.errorSummary => array<(string, JSON.t)>
  let isAborted: SharedLoggerTypes.errorSummary => bool
  let summarizeErrorResponse: 'a => option<SharedLoggerTypes.errorSummary>
  let summarizeExn: exn => SharedLoggerTypes.errorSummary
  let summarizeUnknown: 'a => SharedLoggerTypes.errorSummary
}
module type Host = {
  type sourceType
  type contextType
  type paymentMethodType
  type resourceType
  let sourceName: sourceType => string
  let initialSource: string
  let metadata: SharedLoggerPlatform.metadata
  let version: unit => string
  let mutableDevice: bool
  let notifyOnEmit: bool
  let detailsBeforeFailure: bool
  let enabled: unit => bool
  let minimumRank: unit => int
  let maxRowsPerEventName: unit => int
  let maxUserEventsPerName: unit => int
  let currentContext: unit => contextType
  let fillContext: (contextType, contextType) => contextType
  let identifiers: contextType => SharedLoggerPlatform.identity
  let onSessionChange: ref<unit => unit>
  let qualifiedPayment: paymentMethodType => string
  let droppedRows: ref<int>
  let sendFailures: ref<int>
  let push: (row, ~isError: bool, ~context: contextType) => unit
  let rethrow: exn => 'value
  let resourceName: resourceType => string
  let loadResource: (
    ~url: string,
    ~resource: resourceType,
    ~attributes: array<(string, string)>=?,
    ~matchQuery: bool=?,
    ~onStart: unit => unit,
    ~onLoad: bool => unit,
    ~onError: exn => unit,
  ) => unit
}
module Make = (LoggerConfig: Config, LoggerUtils: Utils, Host: Host) => {
  module LoggerContext = {
    type t = Host.contextType
    let current = Host.currentContext
    let fillFrom = Host.fillContext
    let onSessionChange = Host.onSessionChange
  }
  module LoggerPaymentMethod = {
    type paymentMethod = Host.paymentMethodType
    let qualifiedName = Host.qualifiedPayment
  }
  module LoggerQueue = {
    let droppedRows = Host.droppedRows
    let sendFailures = Host.sendFailures
    let push = Host.push
  }
  let sourceName = Host.sourceName
  let currentSource = ref(Host.initialSource)
  type device = {version: string, appId: string, userAgent: string}
  let device = ref({version: "", appId: "", userAgent: ""})
  let configure = (~source, ~version, ~appId, ~userAgent) => {
    currentSource := source->sourceName
    device := {
        version,
        appId,
        userAgent: userAgent->LoggerUtils.truncateTo(LoggerConfig.maxRowTextLength),
      }
  }
  let isMerchantWindow = () => currentSource.contents === "HYPER_LOADER"
  let isNestedElement = () => currentSource.contents === "ELEMENTS_PAYMENT_METHODS_SDK"
  let platformName = Host.metadata.platformName
  let browserName = Host.metadata.browserName
  let browserVersion = Host.metadata.browserVersion
  let pageHref = Host.metadata.pageHref
  let rank = SharedLoggerRuntime.rank
  let isEnabled = severity => Host.enabled() && severity->rank >= Host.minimumRank()
  let emitCounts = ref(Dict.make())
  let onceKeys = ref(Dict.make())

  let onEmit = ref(() => ())

  LoggerContext.onSessionChange :=
    (
      () => {
        emitCounts := Dict.make()
        onceKeys := Dict.make()
      }
    )

  let emit = (
    ~category,
    ~spec: eventSpec,
    ~severity: severity,
    ~data: option<details>=?,
    ~details: option<details>=?,
    ~verbatim: option<details>=?,
    ~exn: option<exn>=?,
    ~failure: option<'response>=?,
    ~durationMs: option<float>=?,
    ~paymentMethod: option<LoggerPaymentMethod.paymentMethod>=?,
    ~context: option<LoggerContext.t>=?,
    ~source: option<Host.sourceType>=?,
    ~limit: option<int>=?,
    ~message: option<string>=?,
    ~once=false,
    ~dedupKey: option<string>=?,
  ) =>
    if severity->isEnabled {
      LoggerUtils.safeRun(() => {
        let data = data->Option.getOr([])
        let details = details->Option.getOr([])
        let context = switch context {
        | Some(started) => started->LoggerContext.fillFrom(LoggerContext.current())
        | None => LoggerContext.current()
        }
        let identity = Host.identifiers(context)
        let eventName = LoggerUtils.eventName(
          ~category,
          ~action=spec.action,
          ~subject=spec.subject,
          ~outcome=spec.outcome,
        )
        let limit = switch (limit, category) {
        | (None, User) => Some(Host.maxUserEventsPerName())
        | _ => limit
        }
        let cap = limit->Option.getOr(Host.maxRowsPerEventName())
        let eventSeen = emitCounts.contents->Dict.get(eventName)->Option.getOr(0)

        let capReached = limit->Option.isSome && eventSeen === cap
        let (name, severity, data, details, message) = capReached
          ? (
              LoggerUtils.eventName(
                ~category,
                ~action=Fact,
                ~subject="event_limit_reached",
                ~outcome=None,
              ),
              Warning,
              [("limited_event", eventName->JSON.Encode.string), ("limit", cap->JSON.Encode.int)],
              [],
              None,
            )
          : (eventName, severity, data, details, message)
        let seen = capReached ? 0 : eventSeen
        let deduplicate = once || dedupKey->Option.isSome
        let onceKey = switch dedupKey {
        | Some(key) => `${identity.sessionId}#${name}#${key}`
        | None =>
          once
            ? name ++
              "#" ++
              data
              ->Array.concat(details)
              ->Array.concat(verbatim->Option.getOr([]))
              ->Dict.fromArray
              ->JSON.stringifyAny
              ->Option.getOr("")
            : ""
        }
        let duplicateOfOnce = deduplicate && onceKeys.contents->Dict.get(onceKey)->Option.isSome
        if deduplicate {
          onceKeys.contents->Dict.set(onceKey, true)
        }

        if eventSeen <= cap && !duplicateOfOnce {
          emitCounts.contents->Dict.set(eventName, eventSeen + 1)
          let errorDetails = switch exn {
          | Some(exn) => Some(exn->LoggerUtils.summarizeExn)
          | None => failure->Option.flatMap(LoggerUtils.summarizeErrorResponse)
          }->Option.mapOr([], LoggerUtils.errorDetails)
          let details =
            LoggerUtils.mergeDetails(~data, ~details)
            ->Array.concat(errorDetails)
            ->LoggerUtils.normalizeDetails
            ->Array.concat(verbatim->Option.getOr([]))
          let counter = (key, counter) => {
            let count = counter.contents
            counter := 0
            if count > 0 {
              details->Array.push((key, count->JSON.Encode.int))
            }
          }
          counter("dropped_rows", LoggerQueue.droppedRows)
          counter("send_failures", LoggerQueue.sendFailures)
          if limit->Option.isNone && seen === cap {
            details->Array.push(("rate_limited", true->JSON.Encode.bool))
          }

          let value =
            {
              schemaVersion: LoggerConfig.schemaVersion,
              href: pageHref,
              occurrence: seen + 1,
              message: ?message->Option.map(message =>
                message->LoggerUtils.redact->LoggerUtils.truncate
              ),
              details: details->LoggerUtils.fitToBudget->Dict.fromArray,
            }
            ->rowValueToJson
            ->JSON.stringify

          let {version, appId, userAgent} = Host.mutableDevice
            ? device.contents
            : {version: Host.version(), appId: "", userAgent: Host.metadata.userAgent}
          LoggerQueue.push(
            {
              timestamp: Date.now()->Float.toString,
              logType: (severity :> string),
              component: Host.metadata.component,
              category: (category :> string),
              source: source->Option.mapOr(currentSource.contents, sourceName),
              version,
              value,
              sessionId: identity.sessionId,
              merchantId: identity.merchantId,
              paymentId: identity.paymentId,
              authenticationId: identity.authenticationId,
              appId,
              platform: platformName,
              userAgent,
              eventName: name,
              browserName,
              browserVersion,
              latency: durationMs->Option.mapOr("", value => value->Float.toString),
              firstEvent: seen === 0 ? "true" : "false",
              paymentMethod: paymentMethod->Option.mapOr("", LoggerPaymentMethod.qualifiedName),
            },
            ~isError=severity === Error,
            ~context,
          )
          if Host.notifyOnEmit {
            onEmit.contents()
          }
        }
      })
    }

  let step = SharedLoggerRuntime.step

  let emitStep = (
    ~category,
    ~spec: eventSpec,
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
      step.error->Option.mapOr(false, LoggerUtils.isAborted) ? severity.aborted : severity.failure
    }
    let outcomeDetails = []
    step.durationMs->Option.forEach(durationMs =>
      outcomeDetails->Array.push(("duration_ms", durationMs->JSON.Encode.float))
    )
    step.failureClass->Option.forEach(class =>
      outcomeDetails->Array.push(("failure_class", (class :> string)->JSON.Encode.string))
    )
    step.error->Option.forEach(error =>
      outcomeDetails->Array.pushMany(error->LoggerUtils.errorDetails)
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
    ~message=?,
    ~context=?,
    ~source=?,
  ) => {
    let durationMs = startedAt->Option.mapOr(0., startedAt => Date.now() -. startedAt)
    switch outcome {
    | Started => step(~outcome)
    | Failed =>
      step(
        ~outcome,
        ~durationMs,
        ~failureClass=exn->Option.isSome ? Threw : ReturnedFailure,
        ~error=?exn->Option.map(LoggerUtils.summarizeUnknown),
      )
    | outcome => step(~outcome, ~durationMs)
    }->emitStep(
      ~category,
      ~spec,
      ~severity,
      ~data?,
      ~details?,
      ~paymentMethod?,
      ~message?,
      ~context?,
      ~source?,
    )
  }

  type tracker = SharedLoggerRuntime.tracker

  let makeTracker = SharedLoggerRuntime.makeTracker
  let elapsed = SharedLoggerRuntime.elapsed
  let settle = SharedLoggerRuntime.settle
  let isThenable = SharedLoggerRuntime.isThenable

  external asPromise: 'value => promise<'result> = "%identity"
  external asResult: 'value => 'result = "%identity"

  let rethrow = Host.rethrow

  let observe = (
    ~category,
    ~spec: eventSpec,
    ~severity: operationSeverity,
    ~data=?,
    ~details=?,
    ~timeoutMs=LoggerConfig.defaultTimeoutMs,
    ~failureOf: option<'result => option<errorSummary>>=?,
    ~detailsOf: option<'result => details>=?,
    ~paymentMethod=?,
    ~source=?,
    ~syncOutcome=Returned,
    ~message=?,
    ~onStep: option<(step, details) => unit>=?,
    ~call: unit => 'value,
    ~context=LoggerContext.current(),
  ): 'value => {
    let base = LoggerUtils.mergeDetails(
      ~data=data->Option.getOr([]),
      ~details=details->Option.getOr([]),
    )
    let tracker = makeTracker()
    let timedOut = ref(false)

    let emitStep = (step, ~details=[]) => {
      let details = timedOut.contents
        ? details->Array.concat([("settled_after_timeout", true->JSON.Encode.bool)])
        : details
      if Host.notifyOnEmit {
        LoggerUtils.safeRun(() => onStep->Option.forEach(onStep => onStep(step, details)))
      }
      step->emitStep(
        ~category,
        ~spec,
        ~severity,
        ~data=base,
        ~details,
        ~paymentMethod?,
        ~context,
        ~source?,
        ~message?,
      )
    }

    let failed = (~details=[], failureClass, error) =>
      emitStep(step(~outcome=Failed, ~durationMs=tracker->elapsed, ~failureClass, ~error), ~details)

    let settleOrLate = () => tracker->settle || timedOut.contents

    let finish = (result, outcome) =>
      if Host.detailsBeforeFailure {
        let details = try detailsOf->Option.mapOr([], detailsOf => detailsOf(result)) catch {
        | _ => []
        }
        switch try failureOf->Option.flatMap(failureOf => failureOf(result)) catch {
        | error => Some(error->LoggerUtils.summarizeExn)
        } {
        | Some(error) => failed(~details, ReturnedFailure, error)
        | None => emitStep(step(~outcome, ~durationMs=tracker->elapsed), ~details)
        }
      } else {
        switch try failureOf->Option.flatMap(failureOf => failureOf(result)) catch {
        | error => Some(error->LoggerUtils.summarizeExn)
        } {
        | Some(error) => failed(ReturnedFailure, error)
        | None =>
          emitStep(
            step(~outcome, ~durationMs=tracker->elapsed),
            ~details=try detailsOf->Option.mapOr([], detailsOf => detailsOf(result)) catch {
            | _ => []
            },
          )
        }
      }

    try {
      let value = call()
      if value->isThenable {
        emitStep(step(~outcome=Started))
        tracker.timer = Some(setTimeout(() =>
            if tracker->settle {
              timedOut := true
              emitStep(step(~outcome=TimedOut, ~durationMs=tracker->elapsed, ~timeoutMs))
            }
          , timeoutMs))
        value
        ->asPromise
        ->Promise.then(result => {
          if settleOrLate() {
            result->finish(Done)
          }
          Promise.resolve()
        })
        ->Promise.catch(error => {
          if settleOrLate() {
            let error = error->LoggerUtils.summarizeExn
            failed(error->LoggerUtils.isAborted ? Aborted : Rejected, error)
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
          failed(Threw, error->LoggerUtils.summarizeExn)
        }
        rethrow(error)
      }
    }
  }

  // Returns a function with the same type as `fn` that forwards every argument
  // (and `this`) through `run`. Arity-agnostic, so one wrapper serves handlers
  // of any argument count. Non-functions are returned untouched, and the
  // original `length` is preserved for SDKs that inspect handler arity.
  let forwardInvocation = SharedLoggerRuntime.forwardInvocation

  let observeCallback = (
    ~category,
    ~spec,
    ~severity,
    ~data=?,
    ~details=?,
    ~timeoutMs=?,
    ~failureOf=?,
    ~paymentMethod=?,
    ~source=?,
    ~message=?,
    ~callback: 'fn,
    ~context=?,
  ): 'fn =>
    callback->forwardInvocation(invoke =>
      observe(
        ~category,
        ~spec,
        ~severity,
        ~data?,
        ~details?,
        ~timeoutMs?,
        ~failureOf?,
        ~paymentMethod?,
        ~source?,
        ~syncOutcome=Triggered,
        ~message?,
        ~call=invoke,
        ~context?,
      )
    )

  let observeResource = (
    ~spec: eventSpec,
    ~severity,
    ~url,
    ~resource,
    ~attributes=?,
    ~matchQuery=?,
    ~paymentMethod=?,
    ~context=LoggerContext.current(),
    ~source=?,
    ~abandoned=?,
    ~message=?,
    ~onLoad=?,
    ~onError=?,
  ) => {
    let onLoad = onLoad->Option.getOr(() => ())
    let onError = onError->Option.getOr(_ => ())
    let tracker = makeTracker()
    let started = ref(false)

    let emitStep = (step, ~details=[]) =>
      step->emitStep(
        ~category=Resource,
        ~spec,
        ~severity,
        ~data=[
          ("url", url->JSON.Encode.string),
          ("resource_type", resource->Host.resourceName->JSON.Encode.string),
        ],
        ~details,
        ~paymentMethod?,
        ~context,
        ~source?,
        ~message?,
      )

    let start = () =>
      if !started.contents {
        started := true
        emitStep(step(~outcome=Started))
      }

    let failed = (~failureClass, error) =>
      if tracker->settle {
        start()
        emitStep(
          step(
            ~outcome=Failed,
            ~durationMs=tracker->elapsed,
            ~failureClass,
            ~error=error->LoggerUtils.summarizeExn,
          ),
        )
      }

    tracker.timer = Some(setTimeout(() =>
        if tracker->settle {
          start()
          emitStep(
            step(
              ~outcome=TimedOut,
              ~durationMs=tracker->elapsed,
              ~timeoutMs=LoggerConfig.defaultTimeoutMs,
            ),
            ~details=abandoned->Option.mapOr(false, abandoned => abandoned())
              ? [("abandoned", true->JSON.Encode.bool)]
              : [],
          )
        }
      , LoggerConfig.defaultTimeoutMs))

    try Host.loadResource(
      ~url,
      ~resource,
      ~attributes?,
      ~matchQuery?,
      ~onStart=start,
      ~onLoad=result => {
        if tracker->settle {
          switch result {
          | true => emitStep(step(~outcome=Reused, ~durationMs=tracker->elapsed))
          | false => {
              start()
              emitStep(step(~outcome=Done, ~durationMs=tracker->elapsed))
            }
          }
        }
        onLoad()
      },
      ~onError=error => {
        failed(~failureClass=LoadFailed, error)
        onError(error)
      },
    ) catch {
    | error => {
        failed(~failureClass=Threw, error)
        rethrow(error)
      }
    }
  }
}
