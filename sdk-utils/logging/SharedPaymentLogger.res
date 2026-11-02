include PaymentLoggerEvents

let logLifecycle = (~event: lifecycleEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logLifecycle(~event, ~severity=event->lifecycleSeverity, ~dataOf, ~emit)
