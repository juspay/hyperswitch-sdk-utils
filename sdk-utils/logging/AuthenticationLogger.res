open SharedLoggerTypes
include AuthenticationLoggerEvents

let logLifecycle = (
  ~event: lifecycleEvent,
  ~details=?,
  ~exn=?,
  ~failure=?,
  ~durationMs=?,
  ~paymentMethod=?,
  ~context=?,
  ~message=?,
) =>
  SharedAuthenticationLogger.logLifecycle(
    ~event,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(
        ~category, ~spec, ~severity, ~data, ~details?, ~exn?, ~failure?,
        ~durationMs?, ~paymentMethod?, ~context?, ~message?,
      ),
  )

let logUser = (~event: userEvent, ~details=?, ~paymentMethod=?) =>
  SharedAuthenticationLogger.logUser(
    ~event,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data, ~details?, ~paymentMethod?),
  )

let observeApi = (~event: apiEvent, ~url, ~details=?, ~call) =>
  SharedAuthenticationLogger.observeApi(
    ~event,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observe(
        ~category, ~spec, ~severity,
        ~data=data->Array.concat([("url", url->JSON.Encode.string)]), ~details?,
        ~failureOf=SharedLoggerRuntime.httpBodyFailure,
        ~detailsOf=SharedLoggerRuntime.httpBodyDetails, ~call,
      ),
  )
