include ClickToPayLoggerEvents

let logLifecycle = (~event: lifecycleEvent, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logState = (~event: stateEvent, ~emit) =>
  SharedLoggerProducer.logState(~event, ~severity=event->stateSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logUser = (~event: userEvent, ~emit) =>
  SharedLoggerProducer.logUser(~event, ~severity=event->userSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let observeFunction = (~event: functionEvent, ~run) =>
  SharedLoggerProducer.observeFunction(~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let observeMerchantCall = (~method: merchantMethod, ~run) =>
  SharedLoggerProducer.observeMerchantCall(~event=method, ~severity=method->merchantCallSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let observeApi = (~event: apiEvent, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let observeResource = (~event: resourceEvent, ~run) =>
  SharedLoggerProducer.resource(~event, ~severity=event->resourceSeverity, ~run)
