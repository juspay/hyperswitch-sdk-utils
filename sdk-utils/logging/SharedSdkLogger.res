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

let logLifecycle = (~event: lifecycleEvent, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logState = (~event: stateEvent, ~emit) =>
  SharedLoggerProducer.logState(~event, ~severity=event->stateSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logUser = (~event: userEvent, ~emit) => {
  let event = event->normalizeUserEvent
  SharedLoggerProducer.logUser(~event, ~severity=event->userSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)
}

let logCrash = (~origin: crashOrigin, ~emit) =>
  SharedLoggerProducer.logCrash(~event=origin, ~severity=origin->crashSeverity, ~emit)

let observeApi = (~event: apiEvent, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let observeStaticAsset = (~event: staticAssetEvent, ~run) =>
  SharedLoggerProducer.observeResource(~event, ~severity=event->staticAssetSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let logApi = (~event: apiEvent, ~emit) =>
  SharedLoggerProducer.logApi(~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logFunction = (~event: functionEvent, ~emit) =>
  SharedLoggerProducer.logFunction(~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let observeFunction = (~event: functionEvent, ~run) =>
  SharedLoggerProducer.observeFunction(~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let observeFunctionCallback = (~event: functionCallbackEvent, ~run) =>
  SharedLoggerProducer.observeFunctionCallback(~event, ~severity=event->functionCallbackSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let observeResource = (~event: resourceEvent, ~run) =>
  SharedLoggerProducer.resource(~event, ~severity=event->resourceSeverity, ~run)
