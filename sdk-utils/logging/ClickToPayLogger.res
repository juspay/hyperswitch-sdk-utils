open SharedLoggerTypes
include ClickToPayLoggerEvents

let paymentMethod = SharedLoggerPaymentMethod.Card

let source = () =>
  SharedLoggerRuntime.isMerchantWindow() ? Some(SharedLoggerRuntime.AuthenticationSession) : None

let logLifecycle = (~event: lifecycleEvent, ~details=?, ~exn=?, ~message=?) =>
  SharedLoggerProducer.logLifecycle(
    ~event, ~severity=event->lifecycleSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(
        ~category, ~spec, ~severity, ~data, ~details?, ~exn?, ~paymentMethod,
        ~source=?source(), ~message?,
      ),
  )

let logState = (~event: stateEvent) =>
  SharedLoggerProducer.logState(
    ~event, ~severity=event->stateSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data, ~paymentMethod, ~source=?source()),
  )

let logUser = (~event: userEvent) =>
  SharedLoggerProducer.logUser(
    ~event, ~severity=event->userSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data, ~paymentMethod, ~source=?source()),
  )

let observe = (
  ~category,
  ~spec,
  ~severity,
  ~data,
  ~details=?,
  ~failureOf,
  ~detailsOf=?,
  ~message=?,
  ~call,
) =>
  SharedLoggerRuntime.observe(~category, ~spec, ~severity, ~data, ~details?, ~failureOf,
    ~detailsOf?, ~paymentMethod, ~source=?source(), ~message?, ~call)

let observeFunction = (~event: functionEvent, ~detailsOf=?, ~message=?, ~call) =>
  SharedLoggerProducer.observeFunction(
    ~event, ~severity=event->functionSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      observe(~category, ~spec, ~severity, ~data,
        ~failureOf=SharedLoggerRuntime.summarizeErrorResponse, ~detailsOf?, ~message?, ~call),
  )

let observeMerchantCall = (~method: merchantMethod, ~details=?, ~detailsOf=?, ~message=?, ~call) =>
  SharedLoggerProducer.observeMerchantCall(
    ~event=method, ~severity=method->merchantCallSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      observe(~category, ~spec, ~severity,
        ~data=[("surface", "AUTHENTICATION_SESSION"->JSON.Encode.string)], ~details?,
        ~failureOf=SharedLoggerRuntime.summarizeErrorResponse, ~detailsOf?, ~message?, ~call),
  )

let observeApi = (~event: apiEvent, ~url, ~message=?, ~call) =>
  SharedLoggerProducer.observeApi(
    ~event, ~severity=event->apiSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      observe(~category, ~spec, ~severity,
        ~data=data->Array.concat([("url", url->JSON.Encode.string)]),
        ~failureOf=SharedLoggerRuntime.httpFailure, ~detailsOf=SharedLoggerRuntime.httpDetails,
        ~message?, ~call),
  )

let observeResource = (
  ~event: resourceEvent,
  ~url,
  ~attributes=?,
  ~matchQuery=?,
  ~message=?,
  ~onLoad=?,
  ~onError=?,
) =>
  SharedLoggerProducer.resource(
    ~event, ~severity=event->resourceSeverity,
    ~run=(~spec, ~severity) =>
      SharedLoggerRuntime.observeResource(~spec, ~severity, ~url,
        ~resource=event->isStylesheet ? SharedLoggerRuntime.Stylesheet : SharedLoggerRuntime.Script,
        ~attributes?, ~matchQuery?, ~paymentMethod, ~source=?source(),
        ~message?, ~onLoad?, ~onError?),
  )
