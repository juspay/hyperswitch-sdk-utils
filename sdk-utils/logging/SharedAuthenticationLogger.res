include AuthenticationLoggerEvents

let logLifecycle = (~event: lifecycleEvent, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logUser = (~event: userEvent, ~emit) =>
  SharedLoggerProducer.logUser(~event, ~severity=event->userSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let observeApi = (~event: apiEvent, ~run) =>
  SharedLoggerProducer.observeApi(~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)
