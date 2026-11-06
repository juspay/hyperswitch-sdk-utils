open SharedLoggerTypes
include ClickToPayLoggerEvents

let paymentMethod = SharedLoggerPaymentMethod.Card

let logLifecycle = (
  ~event: lifecycleEvent,
  ~details=?,
  ~exn=?,
  ~context=?,
  ~source=?,
  ~message=?,
) =>
  SharedLoggerProducer.logLifecycle(
    ~event,
    ~severity=event->lifecycleSeverity,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(
        ~category,
        ~spec,
        ~severity,
        ~data,
        ~details?,
        ~exn?,
        ~paymentMethod,
        ~context?,
        ~source?,
        ~message?,
      ),
  )

let logState = (~event: stateEvent, ~context=?, ~source=?) =>
  SharedLoggerProducer.logState(
    ~event,
    ~severity=event->stateSeverity,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(
        ~category,
        ~spec,
        ~severity,
        ~data,
        ~paymentMethod,
        ~context?,
        ~source?,
      ),
  )

let logUser = (~event: userEvent, ~context=?, ~source=?) =>
  SharedLoggerProducer.logUser(
    ~event,
    ~severity=event->userSeverity,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(
        ~category,
        ~spec,
        ~severity,
        ~data,
        ~paymentMethod,
        ~context?,
        ~source?,
      ),
  )

let observe = (
  ~category,
  ~spec,
  ~severity,
  ~data,
  ~details=?,
  ~failureOf,
  ~detailsOf=?,
  ~context=?,
  ~source=?,
  ~message=?,
  ~call,
) =>
  SharedLoggerRuntime.observe(
    ~category,
    ~spec,
    ~severity,
    ~data,
    ~details?,
    ~failureOf,
    ~detailsOf?,
    ~paymentMethod,
    ~context?,
    ~source?,
    ~message?,
    ~call,
  )

let observeFunction = (
  ~event: functionEvent,
  ~detailsOf=?,
  ~context=?,
  ~source=?,
  ~message=?,
  ~call,
) =>
  SharedLoggerProducer.observeFunction(
    ~event,
    ~severity=event->functionSeverity,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      observe(
        ~category,
        ~spec,
        ~severity,
        ~data,
        ~context?,
        ~source?,
        ~failureOf=SharedLoggerRuntime.summarizeErrorResponse,
        ~detailsOf?,
        ~message?,
        ~call,
      ),
  )

let observeMerchantCall = (
  ~method: merchantMethod,
  ~surface=?,
  ~details=?,
  ~detailsOf=?,
  ~context=?,
  ~source=?,
  ~message=?,
  ~call,
) =>
  SharedLoggerProducer.observeMerchantCall(
    ~event=method,
    ~severity=method->merchantCallSeverity,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      observe(
        ~category,
        ~spec,
        ~severity,
        ~data=data->Array.concat(
          surface->Option.mapOr([], surface => [("surface", surface->JSON.Encode.string)]),
        ),
        ~details?,
        ~context?,
        ~source?,
        ~failureOf=SharedLoggerRuntime.summarizeErrorResponse,
        ~detailsOf?,
        ~message?,
        ~call,
      ),
  )

let observeApi = (
  ~event: apiEvent,
  ~url,
  ~request: Fetch.Request.init={},
  ~context=?,
  ~source=?,
  ~message=?,
) =>
  SharedLoggerProducer.observeApi(
    ~event,
    ~severity=event->apiSeverity,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      observe(
        ~category,
        ~spec,
        ~severity,
        ~data=data->Array.concat([("url", url->JSON.Encode.string)]),
        ~context?,
        ~source?,
        ~failureOf=SharedLoggerRuntime.httpFailure,
        ~detailsOf=SharedLoggerRuntime.httpDetails,
        ~message?,
        ~call=() => Fetch.fetch(url, request),
      ),
  )

let observeResource = (
  ~event: resourceEvent,
  ~url,
  ~attributes=?,
  ~matchQuery=?,
  ~inBody=?,
  ~context=?,
  ~source=?,
  ~message=?,
  ~onLoad=?,
  ~onError=?,
) =>
  SharedLoggerProducer.resource(~event, ~severity=event->resourceSeverity, ~run=(
    ~spec,
    ~severity,
  ) =>
    SharedLoggerRuntime.observeResource(
      ~spec,
      ~severity,
      ~url,
      ~resource=event->isStylesheet ? SharedLoggerRuntime.Stylesheet : SharedLoggerRuntime.Script,
      ~attributes?,
      ~matchQuery?,
      ~inBody?,
      ~paymentMethod,
      ~context?,
      ~source?,
      ~message?,
      ~onLoad?,
      ~onError?,
    )
  )
