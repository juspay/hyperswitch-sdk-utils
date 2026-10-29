module NativeFunnel = (
  LoggerRuntime: SharedLoggerControllerTypes.NativeRuntime,
  LoggerContext: SharedLoggerControllerTypes.NativeContext,
  LoggerUtils: SharedLoggerControllerTypes.Utils,
) => {
  module SdkLoggerEvents = SharedLoggerEvents.NativeSdk
  open! SharedLoggerTypes
  open! SdkLoggerEvents

  // Reuse the logger's session/counters; no second global session store or credentials.
  let details = (~context=LoggerContext.current(), ()) => {
    let entries = []
    context.sdkInitAt->Option.forEach(start =>
      entries->Array.push(("sdk_elapsed_ms", (Date.now() -. start)->JSON.Encode.float))
    )
    if context.attemptId !== "" {
      entries->Array.push(("attempt_id", context.attemptId->JSON.Encode.string))
    }
    entries
  }

  let log = (
    ~event: lifecycleEvent,
    ~once=false,
    ~context=LoggerContext.current(),
    ~extra=[],
    ~durationMs=?,
    ~paymentMethod=?,
  ) => {
    let spec = event->LoggerUtils.spec
    let name = LoggerUtils.eventName(
      ~category=Lifecycle,
      ~action=Fact,
      ~subject=spec.subject,
      ~outcome=None,
    )
    if !once || LoggerRuntime.emitCounts.contents->Dict.get(name)->Option.getOr(0) === 0 {
      let data = event->LoggerUtils.eventDetails
      let typedKeys = data->Array.map(((key, _)) => key)
      LoggerRuntime.emit(
        ~category=Lifecycle,
        ~spec,
        ~severity=event->lifecycleSeverity,
        ~data,
        ~details=details(~context, ())
        ->Array.concat(extra)
        ->Array.filter(((key, _)) => !(typedKeys->Array.includes(key))),
        ~durationMs?,
        ~paymentMethod?,
        ~context,
        // Scope confirm outcomes to the actual request, independent of changing timing/details.
        ~dedupKey=?switch event {
        | SdkMounted => Some("session")
        | PaymentConfirmResponse(_) | PaymentConfirmFailed(_) | PaymentConfirmTimedOut(_) =>
          Some(context.attemptId)
        | ThreeDsStarted | DdcStarted | AuthenticationStarted(_) => Some(context.attemptId)
        | _ => None
        },
      )
    }
  }

  let init = (~sdkAuthorization, ~clientSecret) => {
    let context = LoggerContext.current()
    if context.sdkInitAt->Option.isNone && (sdkAuthorization !== "" || clientSecret !== "") {
      let context = {...context, sdkInitAt: Some(Date.now())}
      LoggerContext.context := context
      log(
        ~event=SdkInit({
          credentialType: sdkAuthorization !== "" ? "SDK_AUTHORIZATION" : "CLIENT_SECRET",
        }),
        ~context,
        ~once=true,
      )
    }
  }

  let mounted = () => log(~event=SdkMounted, ~once=true)
  let shimmerShown = () => {
    if (
      LoggerRuntime.emitCounts.contents->Dict.get("lifecycle.sdk_rendered")->Option.getOr(0) === 0
    ) {
      mounted()
      log(~event=SdkShimmerShown, ~once=true)
    }
  }

  let rendered = (~startupApiCount, ~degraded=false) => {
    mounted()
    let context = LoggerContext.current()
    log(
      ~event=SdkRendered({startupApiCount, degraded}),
      ~once=true,
      ~durationMs=?context.sdkInitAt->Option.map(start => Date.now() -. start),
    )
  }

  let interaction = name =>
    log(
      ~event=FirstUserInteraction({interaction: name}),
      ~once=true,
      ~durationMs=?LoggerContext.current().sdkInitAt->Option.map(start => Date.now() -. start),
    )

  let beginConfirm = (~paymentMethod=?) => {
    let current = LoggerContext.current()
    let count =
      LoggerRuntime.emitCounts.contents
      ->Dict.get("lifecycle.payment_confirm_attempted")
      ->Option.getOr(0) + 1
    let context = {...current, attemptId: `${current.sessionId}:confirm:${count->Int.toString}`}
    LoggerContext.context := context
    log(~event=PaymentConfirmAttempted({attemptId: context.attemptId}), ~context, ~paymentMethod?)
    context
  }

  let confirmStep = (~context: LoggerContext.t, ~paymentMethod=?, step: step, resultDetails) => {
    let extra = resultDetails->Array.concat(step.error->Option.mapOr([], LoggerUtils.errorDetails))
    switch step.outcome {
    | Started | Reused => ()
    | TimedOut =>
      log(
        ~event=PaymentConfirmTimedOut({attemptId: context.attemptId}),
        ~context,
        ~extra,
        ~durationMs=?step.durationMs,
        ~paymentMethod?,
      )
    | Failed if step.failureClass !== Some(ReturnedFailure) =>
      log(
        ~event=PaymentConfirmFailed({attemptId: context.attemptId}),
        ~context,
        ~extra,
        ~durationMs=?step.durationMs,
        ~paymentMethod?,
      )
    | outcome => {
        let fields = resultDetails->Dict.fromArray
        log(
          ~event=PaymentConfirmResponse({
            attemptId: context.attemptId,
            outcome: (outcome :> string),
            status: ?(fields->Dict.get("payment_status")->Option.flatMap(JSON.Decode.string)),
            statusCode: ?(
              fields
              ->Dict.get("status_code")
              ->Option.flatMap(JSON.Decode.float)
              ->Option.map(Int.fromFloat)
            ),
          }),
          ~context,
          ~extra,
          ~durationMs=?step.durationMs,
          ~paymentMethod?,
        )
      }
    }
  }
}
module NativeSdk = (
  LoggerRuntime: SharedLoggerControllerTypes.NativeRuntime,
  LoggerContext: SharedLoggerControllerTypes.NativeContext,
  LoggerUtils: SharedLoggerControllerTypes.Utils,
  SdkFunnelLogger: {
    let details: (
      ~context: SharedLoggerPlatform.NativeTypes.context=?,
      unit,
    ) => array<(string, JSON.t)>
    let log: (
      ~event: SharedLoggerEvents.NativeSdk.lifecycleEvent,
      ~once: bool=?,
      ~context: SharedLoggerPlatform.NativeTypes.context=?,
      ~extra: array<(string, JSON.t)>=?,
      ~durationMs: float=?,
      ~paymentMethod: LoggerRuntime.paymentMethodType=?,
    ) => unit
    let init: (~sdkAuthorization: string, ~clientSecret: string) => unit
    let mounted: unit => unit
    let shimmerShown: unit => unit
    let rendered: (~startupApiCount: int, ~degraded: bool=?) => unit
    let interaction: string => unit
    let beginConfirm: (
      ~paymentMethod: LoggerRuntime.paymentMethodType=?,
    ) => SharedLoggerPlatform.NativeTypes.context
    let confirmStep: (
      ~context: SharedLoggerPlatform.NativeTypes.context,
      ~paymentMethod: LoggerRuntime.paymentMethodType=?,
      SharedLoggerTypes.step,
      array<(string, JSON.t)>,
    ) => unit
  },
  Host: {
    type nativeProp
    let adoptProps: nativeProp => unit
    let credentials: nativeProp => SharedLoggerControllerTypes.credentials
    let chainGlobalHandler: ((option<JSON.t>, bool) => unit) => bool
    let hasWindowEvents: unit => bool
    let addWindowEvent: (string, SharedLoggerRuntime.crashEvent => unit) => unit
    let drain: unit => unit
  },
) => {
  module LoggerConfig = SharedLoggerConfig.Native
  module SdkLoggerEvents = SharedLoggerEvents.NativeSdk
  module LoggerQueue = {
    let drain = Host.drain
  }
  open! SharedLoggerTypes
  include SdkLoggerEvents

  let idleTimeoutMs = LoggerConfig.idleTimeoutMs

  let idleTracking = ref(false)
  let idleTimer = ref(None)
  let reportingIdle = ref(false)

  let clearIdleTimer = () =>
    idleTimer.contents->Option.forEach(timer => {
      clearTimeout(timer)
      idleTimer := None
    })

  let stopIdleTracking = () => {
    idleTracking := false
    clearIdleTimer()
  }

  let logLifecycle = (
    ~event: lifecycleEvent,
    ~details=?,
    ~exn=?,
    ~failure=?,
    ~durationMs=?,
    ~paymentMethod=?,
    ~source=?,
    ~context=?,
    ~message=?,
  ) => {
    let startsIdleTracking = event === AppRendered
    switch event {
    | AppRendered => idleTracking := true
    | SdkClosed(_) => stopIdleTracking()
    | _ => ()
    }
    // Legacy mount calls feed the canonical funnel event rather than a second row.
    let event = switch event {
    | AppRendered => SdkMounted
    | event => event
    }
    let requestContext = context->Option.getOr(LoggerContext.current())
    LoggerRuntime.emit(
      ~category=Lifecycle,
      ~spec=event->LoggerUtils.spec,
      ~severity=event->lifecycleSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details=SdkFunnelLogger.details(
        ~context=context->Option.getOr(LoggerContext.current()),
        (),
      )->Array.concat(details->Option.getOr([])),
      ~exn?,
      ~failure?,
      ~durationMs?,
      ~paymentMethod?,
      ~source?,
      ~context?,
      ~message?,
      ~dedupKey=?switch event {
      | SdkMounted => Some("session")
      | PaymentSucceeded(_) | PaymentFailed(_) | PaymentStatusUnknown(_) =>
        Some(`${requestContext.paymentId}#${requestContext.attemptId}`)
      | DdcStarted => Some(requestContext.attemptId)
      | _ => None
      },
    )

    // Mount may already have been logged by the startup shimmer. Still start idle tracking.
    if startsIdleTracking {
      LoggerRuntime.onEmit.contents()
    }
  }

  // While the SDK UI is shown, every row restarts the idle timer; the idle row itself does not.
  let restartIdleTimer = () => {
    clearIdleTimer()
    if idleTracking.contents && !reportingIdle.contents {
      idleTimer := Some(setTimeout(() => {
            idleTimer := None
            reportingIdle := true
            logLifecycle(~event=CustomerWentIdle({idleMs: idleTimeoutMs}))
            reportingIdle := false
          }, idleTimeoutMs))
    }
  }

  LoggerRuntime.onEmit := restartIdleTimer

  let logState = (
    ~event: stateEvent,
    ~details=?,
    ~failure=?,
    ~durationMs=?,
    ~paymentMethod=?,
    ~message=?,
  ) => {
    switch event {
    | PaymentFormCompleted({savedMethod}) =>
      SdkFunnelLogger.log(
        ~event=PaymentDetailsFilled({savedMethod: savedMethod}),
        ~once=true,
        ~paymentMethod?,
      )
    | _ =>
      LoggerRuntime.emit(
        ~category=State,
        ~spec=event->LoggerUtils.spec,
        ~severity=event->stateSeverity,
        ~data=event->LoggerUtils.eventDetails,
        ~details?,
        ~failure?,
        ~durationMs?,
        ~paymentMethod?,
        ~message?,
      )
    }
  }

  let namedField = LoggerUtils.namedField

  let logUser = (~event: userEvent, ~details=?, ~paymentMethod=?, ~message=?) => {
    switch event {
    | FieldFocused(_) | FieldBlurred(_) | PaymentSubmitted({source: MerchantApi}) => ()
    | _ => SdkFunnelLogger.interaction(event->LoggerUtils.variantName)
    }
    let event = switch event {
    | FieldFocused({field}) => FieldFocused({field: field->namedField})
    | FieldBlurred({field}) => FieldBlurred({field: field->namedField})
    | FieldToggled({field, enabled}) => FieldToggled({field: field->namedField, enabled})
    | event => event
    }
    LoggerRuntime.emit(
      ~category=User,
      ~spec=event->LoggerUtils.spec,
      ~severity=event->userSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~paymentMethod?,
      ~message?,
    )
  }

  let logCrash = (~origin: crashOrigin, ~exn=?, ~details=?, ~message=?) =>
    LoggerRuntime.emit(
      ~category=Crash,
      ~spec=origin->LoggerUtils.spec,
      ~severity=origin->crashSeverity,
      ~details?,
      ~exn?,
      ~message?,
    )

  type crashEvent = SharedLoggerRuntime.crashEvent
  let describe = SharedLoggerRuntime.describe
  let stackUrl = SharedLoggerRuntime.stackUrl

  // React Native reports uncaught JS errors through ErrorUtils; the previous
  // handler (React Native's own, Sentry's) still runs afterwards.
  let chainGlobalHandler = Host.chainGlobalHandler
  let hasWindowEvents = Host.hasWindowEvents

  let crashCaptureInstalled = ref(false)

  // The JS runtime only runs SDK code, so every crash is reported (web's ownsDocument).
  let catchGlobalCrashes = () =>
    if !crashCaptureInstalled.contents {
      crashCaptureInstalled := true
      let reporting = ref(false)

      let report = (~origin, ~message, ~source, ~details=[]) =>
        if !reporting.contents {
          reporting := true
          logCrash(
            ~origin,
            ~details=[
              ("error_message", message->JSON.Encode.string),
              ("error_source", source->LoggerUtils.sanitizeUrl->JSON.Encode.string),
            ]->Array.concat(details),
          )
          reporting := false
        }

      let chained = chainGlobalHandler((error, isFatal) => {
        report(
          ~origin=UncaughtError,
          ~message=error->describe,
          ~source=error->stackUrl,
          ~details=[("is_fatal", isFatal->JSON.Encode.bool)],
        )
        if isFatal {
          LoggerQueue.drain()
        }
      })

      if !chained && hasWindowEvents() {
        Host.addWindowEvent("error", (event: crashEvent) =>
          report(
            ~origin=UncaughtError,
            ~message=event.message->describe,
            ~source=event.filename->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
          )
        )
        Host.addWindowEvent("unhandledrejection", (event: crashEvent) =>
          report(
            ~origin=UnhandledRejection,
            ~message=event.reason->describe,
            ~source=event.reason->stackUrl,
          )
        )
      }
    }

  let adoptSessionFromNativeProp = (nativeProp: Host.nativeProp) => {
    LoggerUtils.safeRun(() => {
      Host.adoptProps(nativeProp)
    })
    LoggerUtils.safeRun(() => {
      let credentials = Host.credentials(nativeProp)
      SdkFunnelLogger.init(
        ~sdkAuthorization=credentials.sdkAuthorization,
        ~clientSecret=credentials.clientSecret,
      )
    })
    LoggerUtils.safeRun(catchGlobalCrashes)
  }

  let observeApi = (
    ~event: apiEvent,
    ~url,
    ~details=?,
    ~failureOf,
    ~detailsOf,
    ~paymentMethod=?,
    ~message=?,
    ~call,
  ) => {
    let confirmContext = switch event {
    | ConfirmCall | ConfirmPayoutCall => Some(SdkFunnelLogger.beginConfirm(~paymentMethod?))
    | Authentication => {
        SdkFunnelLogger.log(
          ~event=AuthenticationStarted({operation: "authentication"}),
          ~paymentMethod?,
        )
        None
      }
    | _ => None
    }
    LoggerRuntime.observe(
      ~category=Api,
      ~spec=event->LoggerUtils.spec(~action=Request),
      ~severity=event->apiSeverity,
      ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
      ~details=SdkFunnelLogger.details()->Array.concat(details->Option.getOr([])),
      ~failureOf,
      ~detailsOf,
      ~paymentMethod?,
      ~message?,
      ~onStep=?confirmContext->Option.map(context => (step, details) =>
        SdkFunnelLogger.confirmStep(~context, ~paymentMethod?, step, details)),
      ~call,
    )
  }

  let observeStaticAsset = (~event: staticAssetEvent, ~url, ~details=?, ~message=?, ~call) =>
    LoggerRuntime.observe(
      ~category=Resource,
      ~details?,
      ~spec=event->LoggerUtils.spec(~action=Load),
      ~severity=event->staticAssetSeverity,
      ~data=event
      ->LoggerUtils.eventDetails
      ->Array.concat([
        ("url", url->JSON.Encode.string),
        ("resource_type", "static_asset"->JSON.Encode.string),
      ]),
      ~failureOf=LoggerUtils.httpFailure,
      ~detailsOf=LoggerUtils.httpDetails,
      ~message?,
      ~call,
    )

  let logApi = (
    ~event: apiEvent,
    ~outcome,
    ~details=?,
    ~startedAt=?,
    ~exn=?,
    ~paymentMethod=?,
    ~message=?,
  ) =>
    LoggerRuntime.emitPhase(
      ~category=Api,
      ~spec=event->LoggerUtils.spec(~action=Request),
      ~severity=event->apiSeverity,
      ~outcome,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~startedAt?,
      ~exn?,
      ~paymentMethod?,
      ~message?,
    )

  let logFunction = (
    ~event: functionEvent,
    ~outcome,
    ~details=?,
    ~startedAt=?,
    ~exn=?,
    ~paymentMethod=?,
    ~message=?,
  ) =>
    LoggerRuntime.emitPhase(
      ~category=Function,
      ~spec=event->LoggerUtils.spec(~action=Call),
      ~severity=event->functionSeverity,
      ~outcome,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~startedAt?,
      ~exn?,
      ~paymentMethod?,
      ~message?,
    )

  let observeFunction = (
    ~event: functionEvent,
    ~details=?,
    ~timeoutMs=?,
    ~failureOf=?,
    ~detailsOf=?,
    ~paymentMethod=?,
    ~source=?,
    ~message=?,
    ~call,
  ) =>
    LoggerRuntime.observe(
      ~category=Function,
      ~spec=event->LoggerUtils.spec(~action=Call),
      ~severity=event->functionSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~timeoutMs?,
      ~failureOf?,
      ~detailsOf?,
      ~paymentMethod?,
      ~source?,
      ~message?,
      ~call,
    )

  let observeFunctionCallback = (
    ~event: functionCallbackEvent,
    ~details=?,
    ~timeoutMs=?,
    ~paymentMethod=?,
    ~message=?,
    ~callback,
  ) =>
    LoggerRuntime.observeCallback(
      ~category=Function,
      ~spec=event->LoggerUtils.spec(~action=Callback),
      ~severity=event->functionCallbackSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~timeoutMs?,
      ~paymentMethod?,
      ~message?,
      ~callback,
    )
}
module WebSdk = (
  LoggerRuntime: SharedLoggerControllerTypes.WebRuntime,
  LoggerContext: SharedLoggerControllerTypes.WebContext,
  LoggerUtils: SharedLoggerControllerTypes.Utils,
  Host: {
    let listenParentMessage: (string => unit) => unit
    let sdkOrigins: unit => array<string>
    let addWindowEvent: (string, SharedLoggerRuntime.crashEvent => unit) => unit
    let script: LoggerRuntime.resourceType
    let stylesheet: LoggerRuntime.resourceType
  },
) => {
  module SdkLoggerEvents = {
    include SharedLoggerEvents.WebSdk
    let resourceKind = event =>
      SharedLoggerEvents.WebSdk.resourceKind(
        event,
        ~script=Host.script,
        ~stylesheet=Host.stylesheet,
      )
  }
  open SharedLoggerTypes
  include SdkLoggerEvents

  let logLifecycle = (
    ~event: lifecycleEvent,
    ~details=?,
    ~exn=?,
    ~failure=?,
    ~durationMs=?,
    ~paymentMethod=?,
    ~source=?,
    ~context=?,
    ~message=?,
  ) =>
    LoggerRuntime.emit(
      ~category=Lifecycle,
      ~spec=event->LoggerUtils.spec,
      ~severity=event->lifecycleSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~exn?,
      ~failure?,
      ~durationMs?,
      ~paymentMethod?,
      ~source?,
      ~context?,
      ~message?,
    )

  let logState = (
    ~event: stateEvent,
    ~details=?,
    ~failure=?,
    ~durationMs=?,
    ~paymentMethod=?,
    ~context=?,
    ~source=?,
    ~message=?,
  ) =>
    LoggerRuntime.emit(
      ~category=State,
      ~spec=event->LoggerUtils.spec,
      ~severity=event->stateSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~failure?,
      ~durationMs?,
      ~paymentMethod?,
      ~context?,
      ~source?,
      ~message?,
    )

  let namedField = LoggerUtils.namedField

  let logUser = (~event: userEvent, ~details=?, ~paymentMethod=?, ~message=?) => {
    let event = switch event {
    | FieldFocused({field}) => FieldFocused({field: field->namedField})
    | FieldBlurred({field}) => FieldBlurred({field: field->namedField})
    | FieldToggled({field, enabled}) => FieldToggled({field: field->namedField, enabled})
    | event => event
    }
    LoggerRuntime.emit(
      ~category=User,
      ~spec=event->LoggerUtils.spec,
      ~severity=event->userSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~paymentMethod?,
      ~message?,
    )
  }

  let logCrash = (~origin: crashOrigin, ~exn=?, ~details=?, ~context=?, ~message=?) =>
    LoggerRuntime.emit(
      ~category=Crash,
      ~spec=origin->LoggerUtils.spec,
      ~severity=origin->crashSeverity,
      ~details?,
      ~exn?,
      ~context?,
      ~message?,
    )

  let adoptSessionFromParent = (~source) => {
    LoggerRuntime.configure(~source, ~version="", ~appId="", ~userAgent="")
    Host.listenParentMessage(data =>
      if data->LoggerContext.mayCarryContext {
        switch JSON.parseExn(data)->JSON.Decode.object {
        | Some(message) => message->LoggerContext.startSessionFromMessage
        | None => ()
        | exception _ => ()
        }
      }
    )
  }

  type crashEvent = SharedLoggerRuntime.crashEvent
  let describe = SharedLoggerRuntime.describe
  let stackUrl = SharedLoggerRuntime.stackUrl

  let catchGlobalCrashes = (~ownsDocument) => {
    let sdkOrigins = Host.sdkOrigins()->Array.filter(value => value->String.trim !== "")
    let reporting = ref(false)

    let report = (~origin, ~message, ~source) =>
      if (
        !reporting.contents &&
        (ownsDocument ||
        (source->String.trim !== "" &&
          sdkOrigins->Array.some(origin => source->String.startsWith(origin))))
      ) {
        reporting := true
        logCrash(
          ~origin,
          ~details=[
            ("error_message", message->JSON.Encode.string),
            ("error_source", source->LoggerUtils.sanitizeUrl->JSON.Encode.string),
          ],
        )
        reporting := false
      }

    Host.addWindowEvent("error", (event: crashEvent) =>
      report(
        ~origin=UncaughtError,
        ~message=event.message->describe,
        ~source=event.filename->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
      )
    )

    Host.addWindowEvent("unhandledrejection", (event: crashEvent) =>
      report(
        ~origin=UnhandledRejection,
        ~message=event.reason->describe,
        ~source=event.reason->stackUrl,
      )
    )
  }

  let observeApi = (
    ~event: apiEvent,
    ~url,
    ~details=?,
    ~failureOf,
    ~detailsOf,
    ~paymentMethod=?,
    ~context=?,
    ~source=?,
    ~message=?,
    ~call,
  ) =>
    LoggerRuntime.observe(
      ~category=Api,
      ~spec=event->LoggerUtils.spec(~action=Request),
      ~severity=event->apiSeverity,
      ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
      ~details?,
      ~failureOf,
      ~detailsOf,
      ~paymentMethod?,
      ~context?,
      ~source?,
      ~message?,
      ~call,
    )

  let observeStaticAsset = (~event: staticAssetEvent, ~url, ~details=?, ~message=?, ~call) =>
    LoggerRuntime.observe(
      ~category=Resource,
      ~details?,
      ~spec=event->LoggerUtils.spec(~action=Load),
      ~severity=event->staticAssetSeverity,
      ~data=event
      ->LoggerUtils.eventDetails
      ->Array.concat([
        ("url", url->JSON.Encode.string),
        ("resource_type", "static_asset"->JSON.Encode.string),
      ]),
      ~failureOf=LoggerUtils.httpFailure,
      ~detailsOf=LoggerUtils.httpDetails,
      ~message?,
      ~call,
    )

  let logApi = (
    ~event: apiEvent,
    ~outcome,
    ~details=?,
    ~startedAt=?,
    ~exn=?,
    ~paymentMethod=?,
    ~context=?,
    ~message=?,
  ) =>
    LoggerRuntime.emitPhase(
      ~category=Api,
      ~spec=event->LoggerUtils.spec(~action=Request),
      ~severity=event->apiSeverity,
      ~outcome,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~startedAt?,
      ~exn?,
      ~paymentMethod?,
      ~context?,
      ~message?,
    )

  let logFunction = (
    ~event: functionEvent,
    ~outcome,
    ~details=?,
    ~startedAt=?,
    ~exn=?,
    ~paymentMethod=?,
    ~context=?,
    ~message=?,
  ) =>
    LoggerRuntime.emitPhase(
      ~category=Function,
      ~spec=event->LoggerUtils.spec(~action=Call),
      ~severity=event->functionSeverity,
      ~outcome,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~startedAt?,
      ~exn?,
      ~paymentMethod?,
      ~context?,
      ~message?,
    )

  let observeFunction = (
    ~event: functionEvent,
    ~details=?,
    ~timeoutMs=?,
    ~paymentMethod=?,
    ~source=?,
    ~context=?,
    ~message=?,
    ~call,
  ) =>
    LoggerRuntime.observe(
      ~category=Function,
      ~spec=event->LoggerUtils.spec(~action=Call),
      ~severity=event->functionSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~timeoutMs?,
      ~paymentMethod?,
      ~source?,
      ~context?,
      ~message?,
      ~call,
    )

  let observeFunctionCallback = (
    ~event: functionCallbackEvent,
    ~details=?,
    ~timeoutMs=?,
    ~paymentMethod=?,
    ~context=?,
    ~message=?,
    ~callback,
  ) =>
    LoggerRuntime.observeCallback(
      ~category=Function,
      ~spec=event->LoggerUtils.spec(~action=Callback),
      ~severity=event->functionCallbackSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~timeoutMs?,
      ~paymentMethod?,
      ~context?,
      ~message?,
      ~callback,
    )

  let observeResource = (
    ~event: resourceEvent,
    ~url,
    ~attributes=?,
    ~matchQuery=?,
    ~paymentMethod=?,
    ~context=?,
    ~abandoned=?,
    ~message=?,
    ~onLoad=?,
    ~onError=?,
  ) =>
    LoggerRuntime.observeResource(
      ~spec=event->LoggerUtils.spec(~action=Load),
      ~severity=event->resourceSeverity,
      ~url,
      ~resource=event->resourceKind,
      ~context?,
      ~attributes?,
      ~matchQuery?,
      ~paymentMethod?,
      ~abandoned?,
      ~message?,
      ~onLoad?,
      ~onError?,
    )
}
module NativeMerchant = (
  LoggerRuntime: SharedLoggerControllerTypes.NativeRuntime,
  LoggerUtils: SharedLoggerControllerTypes.Utils,
  Host: {
    type nativeProp
    let surfaceOfNativeProp: nativeProp => SharedLoggerEvents.NativeMerchant.surface
    let getDictFromJson: JSON.t => Dict.t<JSON.t>
    let testMode: nativeProp => bool
    let customBackend: nativeProp => option<string>
  },
) => {
  module MerchantLoggerEvents = SharedLoggerEvents.NativeMerchant
  open! SharedLoggerTypes
  include MerchantLoggerEvents

  let surfaceOfNativeProp = Host.surfaceOfNativeProp

  let observeMerchantCall = (
    ~event: merchantCallEvent,
    ~details=?,
    ~timeoutMs=?,
    ~failureOf=LoggerUtils.summarizeErrorResponse,
    ~detailsOf=?,
    ~source=?,
    ~message=?,
    ~call,
  ) =>
    LoggerRuntime.observe(
      ~category=Merchant,
      ~spec=event->LoggerUtils.spec(~action=Call),
      ~severity=event->merchantCallSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~timeoutMs?,
      ~failureOf,
      ~detailsOf?,
      ~source?,
      ~message?,
      ~call,
    )

  let observeMerchantCallback = (
    ~event: merchantCallbackEvent,
    ~timeoutMs=?,
    ~message=?,
    ~callback,
  ) =>
    LoggerRuntime.observeCallback(
      ~category=Merchant,
      ~spec=event->LoggerUtils.spec(~action=Callback),
      ~severity=event->merchantCallbackSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~timeoutMs?,
      ~failureOf=LoggerUtils.summarizeErrorResponse,
      ~message?,
      ~callback,
    )

  let logMerchantCall = (~event: merchantCallEvent, ~details=?, ~message=?) =>
    LoggerRuntime.emit(
      ~category=Merchant,
      ~spec=event->LoggerUtils.spec(~action=Call, ~outcome=Returned),
      ~severity=(event->merchantCallSeverity).success,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~message?,
    )

  let logMerchantProps = (
    ~event: merchantPropEvent,
    ~details=?,
    ~config: option<JSON.t>=?,
    ~isSensitive=LoggerUtils.maskAll,
    ~message=?,
  ) =>
    if event->merchantPropSeverity->LoggerRuntime.isEnabled {
      LoggerRuntime.emit(
        ~category=Merchant,
        ~spec=event->LoggerUtils.spec(~action=Prop),
        ~severity=event->merchantPropSeverity,
        ~data=event->LoggerUtils.eventDetails,
        ~details?,
        ~verbatim=?config->Option.map(config => config->LoggerUtils.configSnapshot(~isSensitive)),
        ~message?,
        ~once=true,
      )
    }

  let logMerchantIssue = (~issue: merchantIssue, ~details=?, ~message=?) =>
    LoggerRuntime.emit(
      ~category=Merchant,
      ~spec=issue->LoggerUtils.spec(~action=IntegrationIssue),
      ~severity=issue->merchantIssueSeverity,
      ~data=issue->LoggerUtils.eventDetails,
      ~details?,
      ~message?,
      ~once=true,
    )

  // Display options are logged as given; every other string (billing and shipping
  // details, customer, API keys) is masked.
  let fieldsToExcludeFromMasking = SharedLoggerConfig.Native.fieldsToExcludeFromMasking
  let shouldMaskField = SharedLoggerConfig.Native.shouldMaskField

  // The native counterpart of web's Hyper/Elements option logging, from the raw props.
  let logNativeProps = (~nativeProp: Host.nativeProp, ~props: JSON.t) => {
    let surface = nativeProp->surfaceOfNativeProp
    let dict = props->Host.getDictFromJson
    let configuration =
      dict->Dict.get("configuration")->Option.flatMap(JSON.Decode.object)->Option.getOr(Dict.make())

    logMerchantProps(
      ~event=TestMode({surface: surface}),
      ~details=[("test_mode", Host.testMode(nativeProp)->JSON.Encode.bool)],
    )
    nativeProp
    ->Host.customBackend
    ->Option.forEach(url =>
      logMerchantProps(
        ~event=CustomBackendUrl({surface: surface}),
        ~details=[("url", url->JSON.Encode.string)],
      )
    )
    configuration
    ->Dict.get("appearance")
    ->Option.forEach(config =>
      logMerchantProps(
        ~event=Appearance({surface: surface}),
        ~config,
        ~isSensitive=LoggerUtils.maskNone,
      )
    )
    configuration
    ->Dict.get("locale")
    ->Option.forEach(config =>
      logMerchantProps(
        ~event=Locale({surface: surface}),
        ~config,
        ~isSensitive=LoggerUtils.maskNone,
      )
    )
    let options = configuration->Dict.toArray->Array.filter(((key, _)) => key !== "appearance")
    if options->Array.length > 0 {
      logMerchantProps(
        ~event=PaymentElementOptions({surface: surface}),
        ~config=options->Dict.fromArray->JSON.Encode.object,
        ~isSensitive=shouldMaskField,
      )
    }
  }
}
module WebMerchant = (
  LoggerRuntime: SharedLoggerControllerTypes.WebRuntime,
  LoggerContext: SharedLoggerControllerTypes.WebContext,
  LoggerUtils: SharedLoggerControllerTypes.Utils,
) => {
  module HyperLoaderLoggerEvents = SharedLoggerEvents.WebMerchant
  open SharedLoggerTypes
  include HyperLoaderLoggerEvents

  let observeMerchantCall = (
    ~event: merchantCallEvent,
    ~details=?,
    ~timeoutMs=?,
    ~failureOf=LoggerUtils.summarizeErrorResponse,
    ~detailsOf=?,
    ~source=?,
    ~context=?,
    ~message=?,
    ~call,
  ) =>
    LoggerRuntime.observe(
      ~category=Merchant,
      ~spec=event->LoggerUtils.spec(~action=Call),
      ~severity=event->merchantCallSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~timeoutMs?,
      ~failureOf,
      ~detailsOf?,
      ~source?,
      ~context?,
      ~message?,
      ~call,
    )

  let observeMerchantCallback = (
    ~event: merchantCallbackEvent,
    ~context=?,
    ~source=?,
    ~timeoutMs=?,
    ~message=?,
    ~callback,
  ) =>
    LoggerRuntime.observeCallback(
      ~category=Merchant,
      ~spec=event->LoggerUtils.spec(~action=Callback),
      ~context?,
      ~source?,
      ~severity=event->merchantCallbackSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~timeoutMs?,
      ~failureOf=LoggerUtils.summarizeErrorResponse,
      ~message?,
      ~callback,
    )

  let logMerchantCall = (~event: merchantCallEvent, ~details=?, ~message=?) =>
    LoggerRuntime.emit(
      ~category=Merchant,
      ~spec=event->LoggerUtils.spec(~action=Call, ~outcome=Returned),
      ~severity=(event->merchantCallSeverity).success,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~message?,
    )

  let logMerchantProps = (
    ~event: merchantPropEvent,
    ~details=?,
    ~config: option<JSON.t>=?,
    ~isSensitive=LoggerUtils.maskAll,
    ~message=?,
  ) =>
    if !LoggerRuntime.isNestedElement() && event->merchantPropSeverity->LoggerRuntime.isEnabled {
      LoggerRuntime.emit(
        ~category=Merchant,
        ~spec=event->LoggerUtils.spec(~action=Prop),
        ~severity=event->merchantPropSeverity,
        ~data=event->LoggerUtils.eventDetails,
        ~details?,
        ~verbatim=?config->Option.map(config => config->LoggerUtils.configSnapshot(~isSensitive)),
        ~message?,
        ~once=true,
      )
    }

  let logMerchantIssue = (~issue: merchantIssue, ~details=?, ~context=?, ~message=?) =>
    if !LoggerRuntime.isNestedElement() {
      LoggerRuntime.emit(
        ~category=Merchant,
        ~spec=issue->LoggerUtils.spec(~action=IntegrationIssue),
        ~context?,
        ~severity=issue->merchantIssueSeverity,
        ~data=issue->LoggerUtils.eventDetails,
        ~details?,
        ~message?,
        ~once=true,
      )
    }

  let startSession = (~sessionId=?, ~merchantId=?) => {
    LoggerContext.setSessionData(~sessionId?, ~merchantId?, ())
  }
}
module WebClickToPay = (
  LoggerRuntime: SharedLoggerControllerTypes.WebRuntime,
  LoggerUtils: SharedLoggerControllerTypes.Utils,
  Host: {
    let card: LoggerRuntime.paymentMethodType
    let authenticationSource: LoggerRuntime.sourceType
    let script: LoggerRuntime.resourceType
    let stylesheet: LoggerRuntime.resourceType
  },
) => {
  module ClickToPayLoggerEvents = {
    include SharedLoggerEvents.WebClickToPay
    let resourceKind = event =>
      SharedLoggerEvents.WebClickToPay.resourceKind(
        event,
        ~script=Host.script,
        ~stylesheet=Host.stylesheet,
      )
  }
  open SharedLoggerTypes
  include ClickToPayLoggerEvents

  let paymentMethod = Host.card

  let source = () => LoggerRuntime.isMerchantWindow() ? Some(Host.authenticationSource) : None

  let logLifecycle = (~event: lifecycleEvent, ~details=?, ~exn=?, ~message=?) =>
    LoggerRuntime.emit(
      ~category=Lifecycle,
      ~spec=event->LoggerUtils.spec,
      ~severity=event->lifecycleSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~details?,
      ~exn?,
      ~paymentMethod,
      ~source=?source(),
      ~message?,
    )

  let observe = (
    ~category,
    ~spec,
    ~severity,
    ~data,
    ~details=?,
    ~failureOf,
    ~detailsOf=?,
    ~message=?,
    ~call,
  ) =>
    LoggerRuntime.observe(
      ~category,
      ~spec,
      ~severity,
      ~data,
      ~details?,
      ~failureOf,
      ~detailsOf?,
      ~paymentMethod,
      ~source=?source(),
      ~message?,
      ~call,
    )

  let observeFunction = (~event: functionEvent, ~detailsOf=?, ~message=?, ~call) =>
    observe(
      ~category=Function,
      ~spec=event->LoggerUtils.spec(~action=Call),
      ~severity=event->functionSeverity,
      ~data=event->LoggerUtils.eventDetails,
      ~failureOf=LoggerUtils.summarizeErrorResponse,
      ~detailsOf?,
      ~message?,
      ~call,
    )

  let observeMerchantCall = (
    ~method: merchantMethod,
    ~details=?,
    ~detailsOf=?,
    ~message=?,
    ~call,
  ) =>
    observe(
      ~category=Merchant,
      ~spec=method->LoggerUtils.spec(~action=Call),
      ~severity=method->merchantCallSeverity,
      ~data=[("surface", "AUTHENTICATION_SESSION"->JSON.Encode.string)],
      ~details?,
      ~failureOf=LoggerUtils.summarizeErrorResponse,
      ~detailsOf?,
      ~message?,
      ~call,
    )

  let observeApi = (~event: apiEvent, ~url, ~message=?, ~call) =>
    observe(
      ~category=Api,
      ~spec=event->LoggerUtils.spec(~action=Request),
      ~severity=event->apiSeverity,
      ~data=event->LoggerUtils.eventDetails->Array.concat([("url", url->JSON.Encode.string)]),
      ~failureOf=LoggerUtils.httpFailure,
      ~detailsOf=LoggerUtils.httpDetails,
      ~message?,
      ~call,
    )

  let observeResource = (
    ~event: resourceEvent,
    ~url,
    ~attributes=?,
    ~matchQuery=?,
    ~message=?,
    ~onLoad=?,
    ~onError=?,
  ) =>
    LoggerRuntime.observeResource(
      ~spec=event->LoggerUtils.spec(~action=Load),
      ~severity=event->resourceSeverity,
      ~url,
      ~resource=event->resourceKind,
      ~attributes?,
      ~matchQuery?,
      ~paymentMethod,
      ~source=?source(),
      ~message?,
      ~onLoad?,
      ~onError?,
    )
}
