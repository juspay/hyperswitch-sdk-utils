include PaymentMethodSessionLoggerEvents

let logState = (~event: stateEvent, ~emit) =>
  SharedLoggerProducer.logState(~event, ~severity=event->stateSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let observeApi = (~event: apiEvent, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let logLifecycle = (~event: lifecycleEvent, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logFunction = (~event: functionEvent, ~emit) =>
  SharedLoggerProducer.logFunction(~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let observeFunctionCallback = (~event: functionCallbackEvent, ~run) =>
  SharedLoggerProducer.observeFunctionCallback(~event, ~severity=event->functionCallbackSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let logApi = (~event: apiEvent, ~emit) =>
  SharedLoggerProducer.logApi(~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let observeResource = (~event: resourceEvent, ~run) =>
  SharedLoggerProducer.resource(~event, ~severity=event->resourceSeverity, ~run)
