open SharedLoggerTypes
include SdkLoggerEvents

let namedField = field =>
  switch field->String.trim {
  | "" => "unnamed"
  | "cardNoInput" => "card_number"
  | "expiryInput" => "card_expiry"
  | "cvvInput" => "card_cvc"
  | field => field->SharedLoggerEvent.snakeCase
  }

let normalizeUserEvent = event =>
  switch event {
  | FieldFocused({field}) => FieldFocused({field: field->namedField})
  | FieldBlurred({field}) => FieldBlurred({field: field->namedField})
  | FieldToggled({field, enabled}) => FieldToggled({field: field->namedField, enabled})
  | event => event
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
) =>
  SharedLoggerProducer.logLifecycle(
    ~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data,
        ~details?, ~exn?, ~failure?, ~durationMs?, ~paymentMethod?, ~source?, ~context?, ~message?),
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
  SharedLoggerProducer.logState(
    ~event, ~severity=event->stateSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data,
        ~details?, ~failure?, ~durationMs?, ~paymentMethod?, ~context?, ~source?, ~message?),
  )

let logUser = (~event: userEvent, ~details=?, ~paymentMethod=?, ~message=?) => {
  let event = event->normalizeUserEvent
  SharedLoggerProducer.logUser(
    ~event, ~severity=event->userSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data, ~details?, ~paymentMethod?, ~message?),
  )
}

let logCrash = (~origin: crashOrigin, ~exn=?, ~details=?, ~context=?, ~message=?) =>
  SharedLoggerProducer.logCrash(
    ~event=origin, ~severity=origin->crashSeverity,
    ~emit=(~category, ~spec, ~severity) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~details?, ~exn?, ~context?, ~message?),
  )

let observeApi = (
  ~event: apiEvent,
  ~url,
  ~request: Fetch.Request.init={},
  ~details=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~message=?,
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
        ~paymentMethod?,
        ~context?,
        ~source?,
        ~message?,
        ~call=() => Fetch.fetch(url, request),
      ),
  )

let observeStaticAsset = (
  ~event: staticAssetEvent,
  ~url,
  ~request: Fetch.Request.init={},
  ~details=?,
  ~message=?,
) =>
  SharedLoggerProducer.observeResource(
    ~event, ~severity=event->staticAssetSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observe(~category, ~spec, ~severity,
        ~data=data->Array.concat([
          ("url", url->JSON.Encode.string),
          ("resource_type", "static_asset"->JSON.Encode.string),
        ]), ~details?,
        ~failureOf=SharedLoggerRuntime.httpFailure, ~detailsOf=SharedLoggerRuntime.httpDetails,
        ~message?, ~call=() => Fetch.fetch(url, request)),
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
  SharedLoggerProducer.logApi(
    ~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emitPhase(~category, ~spec, ~severity, ~data, ~outcome,
        ~details?, ~startedAt?, ~exn?, ~paymentMethod?, ~context?, ~message?),
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
  SharedLoggerProducer.logFunction(
    ~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emitPhase(~category, ~spec, ~severity, ~data, ~outcome,
        ~details?, ~startedAt?, ~exn?, ~paymentMethod?, ~context?, ~message?),
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
  SharedLoggerProducer.observeFunction(
    ~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observe(~category, ~spec, ~severity, ~data, ~details?,
        ~timeoutMs?, ~paymentMethod?, ~source?, ~context?, ~message?, ~call),
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
  SharedLoggerProducer.observeFunctionCallback(
    ~event, ~severity=event->functionCallbackSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observeCallback(~category, ~spec, ~severity, ~data,
        ~details?, ~timeoutMs?, ~paymentMethod?, ~context?, ~message?, ~callback),
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
  SharedLoggerProducer.resource(
    ~event, ~severity=event->resourceSeverity,
    ~run=(~spec, ~severity) =>
      SharedLoggerRuntime.observeResource(~spec, ~severity, ~url,
        ~resource=event->isStylesheet ? SharedLoggerRuntime.Stylesheet : SharedLoggerRuntime.Script,
        ~context?, ~attributes?, ~matchQuery?, ~paymentMethod?, ~abandoned?,
        ~message?, ~onLoad?, ~onError?),
  )
