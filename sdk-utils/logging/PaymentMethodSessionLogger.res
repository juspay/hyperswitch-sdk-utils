open SharedLoggerTypes
include PaymentMethodSessionLoggerEvents

let logState = (~event: stateEvent, ~context=?, ~durationMs=?) =>
  SharedPaymentMethodSessionLogger.logState(
    ~event,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data, ~context?, ~durationMs?),
  )

let observeApi = (
  ~event: apiEvent,
  ~url,
  ~context=?,
  ~details=?,
  ~failureOf,
  ~detailsOf,
  ~call,
) =>
  SharedPaymentMethodSessionLogger.observeApi(
    ~event,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observe(~category, ~spec, ~severity,
        ~data=data->Array.concat([("url", url->JSON.Encode.string)]),
        ~details?, ~failureOf, ~detailsOf, ~context?, ~call),
  )

let logLifecycle = (
  ~event: lifecycleEvent,
  ~context=?,
  ~details=?,
  ~exn=?,
  ~failure=?,
  ~paymentMethod=?,
  ~message=?,
) =>
  SharedPaymentMethodSessionLogger.logLifecycle(
    ~event,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data,
        ~context?, ~details?, ~exn?, ~failure?, ~paymentMethod?, ~message?),
  )

let logFunction = (
  ~event: functionEvent,
  ~outcome,
  ~context=?,
  ~startedAt=?,
  ~exn=?,
  ~paymentMethod=SharedLoggerPaymentMethod.Card,
  ~message=?,
) =>
  SharedPaymentMethodSessionLogger.logFunction(
    ~event,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emitPhase(~category, ~spec, ~severity, ~data, ~outcome,
        ~context?, ~startedAt?, ~exn?, ~paymentMethod, ~message?),
  )

let observeFunctionCallback = (~event: functionCallbackEvent, ~context=?, ~paymentMethod=?, ~callback) =>
  SharedPaymentMethodSessionLogger.observeFunctionCallback(
    ~event,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observeCallback(~category, ~spec, ~severity, ~data,
        ~context?, ~paymentMethod?, ~callback),
  )

let logApi = (
  ~event: apiEvent,
  ~outcome,
  ~context=?,
  ~details=?,
  ~startedAt=?,
  ~exn=?,
  ~paymentMethod=SharedLoggerPaymentMethod.Card,
  ~message=?,
) =>
  SharedPaymentMethodSessionLogger.logApi(
    ~event,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emitPhase(~category, ~spec, ~severity, ~data, ~outcome,
        ~context?, ~details?, ~startedAt?, ~exn?, ~paymentMethod, ~message?),
  )

let observeResource = (
  ~event: resourceEvent,
  ~url,
  ~attributes=?,
  ~matchQuery=?,
  ~context=?,
  ~abandoned=?,
  ~onLoad=?,
  ~onError=?,
) =>
  SharedPaymentMethodSessionLogger.observeResource(
    ~event,
    ~run=(~spec, ~severity) =>
      SharedLoggerRuntime.observeResource(~spec, ~severity, ~url, ~resource=SharedLoggerRuntime.Script,
        ~attributes?, ~matchQuery?, ~context?, ~abandoned?, ~onLoad?, ~onError?),
  )
