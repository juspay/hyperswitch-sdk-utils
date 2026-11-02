include AuthenticationLoggerEvents

let logLifecycle = (~event: lifecycleEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf, ~emit)

let logUser = (~event: userEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logUser(~event, ~severity=event->userSeverity, ~dataOf, ~emit)

let observeApi = (~event: apiEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf, ~run)
