include ClickToPayLoggerEvents

let logLifecycle = (~event: lifecycleEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf, ~emit)

let logState = (~event: stateEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logState(~event, ~severity=event->stateSeverity, ~dataOf, ~emit)

let logUser = (~event: userEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logUser(~event, ~severity=event->userSeverity, ~dataOf, ~emit)

let observeFunction = (~event: functionEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeFunction(~event, ~severity=event->functionSeverity, ~dataOf, ~run)

let observeMerchantCall = (~method: merchantMethod, ~dataOf, ~run) =>
  SharedLoggerProducer.observeMerchantCall(~event=method, ~severity=method->merchantCallSeverity, ~dataOf, ~run)

let observeApi = (~event: apiEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf, ~run)

let observeResource = (~event: resourceEvent, ~run) =>
  SharedLoggerProducer.resource(~event, ~severity=event->resourceSeverity, ~run)
