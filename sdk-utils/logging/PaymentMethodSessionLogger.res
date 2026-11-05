include PaymentMethodSessionLoggerEvents

let logState = (~event: stateEvent, ~context=?, ~durationMs=?) =>
  SharedLoggerProducer.logState(
    ~event, ~severity=event->stateSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data, ~context?, ~durationMs?),
  )

let observeApi = (
  ~event: apiEvent,
  ~url,
  ~request: Fetch.Request.init={},
  ~context=?,
  ~details=?,
) =>
  SharedLoggerProducer.observeApi(
    ~event,
    ~severity=event->apiSeverity,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observe(
        ~category,
        ~spec,
        ~severity,
        ~data=data->Array.concat([("url", url->JSON.Encode.string)]),
        ~details?,
        ~failureOf=SharedLoggerRuntime.httpFailure,
        ~detailsOf=SharedLoggerRuntime.httpDetails,
        ~context?,
        ~call=() => Fetch.fetch(url, request),
      ),
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
  SharedLoggerProducer.logLifecycle(
    ~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
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
  SharedLoggerProducer.logFunction(
    ~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emitPhase(~category, ~spec, ~severity, ~data, ~outcome,
        ~context?, ~startedAt?, ~exn?, ~paymentMethod, ~message?),
  )

let observeFunctionCallback = (~event: functionCallbackEvent, ~context=?, ~paymentMethod=?, ~callback) =>
  SharedLoggerProducer.observeFunctionCallback(
    ~event, ~severity=event->functionCallbackSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
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
  SharedLoggerProducer.logApi(
    ~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
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
  SharedLoggerProducer.resource(
    ~event, ~severity=event->resourceSeverity,
    ~run=(~spec, ~severity) =>
      SharedLoggerRuntime.observeResource(~spec, ~severity, ~url, ~resource=SharedLoggerRuntime.Script,
        ~attributes?, ~matchQuery?, ~context?, ~abandoned?, ~onLoad?, ~onError?),
  )
