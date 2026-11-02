include PaymentMethodSessionLoggerEvents

let logState = (~event: stateEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logState(~event, ~severity=event->stateSeverity, ~dataOf, ~emit)

let observeApi = (~event: apiEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf, ~run)

let logLifecycle = (~event: lifecycleEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf, ~emit)

let logFunction = (~event: functionEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logFunction(~event, ~severity=event->functionSeverity, ~dataOf, ~emit)

let observeFunctionCallback = (~event: functionCallbackEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeFunctionCallback(~event, ~severity=event->functionCallbackSeverity, ~dataOf, ~run)

let logApi = (~event: apiEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logApi(~event, ~severity=event->apiSeverity, ~dataOf, ~emit)

let observeResource = (~event: resourceEvent, ~run) =>
  SharedLoggerProducer.resource(~event, ~severity=event->resourceSeverity, ~run)
