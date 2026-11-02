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

let logLifecycle = (~event: lifecycleEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf, ~emit)

let logState = (~event: stateEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logState(~event, ~severity=event->stateSeverity, ~dataOf, ~emit)

let logUser = (~event: userEvent, ~dataOf, ~emit) => {
  let event = event->normalizeUserEvent
  SharedLoggerProducer.logUser(~event, ~severity=event->userSeverity, ~dataOf, ~emit)
}

let logCrash = (~origin: crashOrigin, ~emit) =>
  SharedLoggerProducer.logCrash(~event=origin, ~severity=origin->crashSeverity, ~emit)

let observeApi = (~event: apiEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf, ~run)

let observeStaticAsset = (~event: staticAssetEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeResource(~event, ~severity=event->staticAssetSeverity, ~dataOf, ~run)

let logApi = (~event: apiEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logApi(~event, ~severity=event->apiSeverity, ~dataOf, ~emit)

let logFunction = (~event: functionEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logFunction(~event, ~severity=event->functionSeverity, ~dataOf, ~emit)

let observeFunction = (~event: functionEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeFunction(~event, ~severity=event->functionSeverity, ~dataOf, ~run)

let observeFunctionCallback = (~event: functionCallbackEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeFunctionCallback(~event, ~severity=event->functionCallbackSeverity, ~dataOf, ~run)

let observeResource = (~event: resourceEvent, ~run) =>
  SharedLoggerProducer.resource(~event, ~severity=event->resourceSeverity, ~run)
