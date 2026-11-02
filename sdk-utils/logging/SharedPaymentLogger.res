include PaymentLoggerEvents

let logLifecycle = (~event: lifecycleEvent, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)
