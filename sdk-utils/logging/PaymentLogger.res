open SharedLoggerTypes
include PaymentLoggerEvents

let logLifecycle = (
  ~event: lifecycleEvent,
  ~details=?,
  ~exn=?,
  ~failure=?,
  ~durationMs=?,
  ~paymentMethod=?,
  ~context=?,
  ~source=?,
  ~message=?,
) =>
  SharedPaymentLogger.logLifecycle(
    ~event,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(
        ~category,
        ~spec,
        ~severity,
        ~data,
        ~details?,
        ~exn?,
        ~failure?,
        ~durationMs?,
        ~paymentMethod?,
        ~context?,
        ~source?,
        ~message?,
      ),
  )
