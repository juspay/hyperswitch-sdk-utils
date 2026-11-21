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
  ~sharedDetailsOf=?,
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
    ~sharedDetailsOf?,
    ~paymentMethod,
    ~context?,
    ~source?,
    ~message?,
    ~call,
  )

let observeFunction = (
  ~event: functionEvent,
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
        ~sharedDetailsOf=result =>
          switch event {
          | IdentityLookup(data) =>
            IdentityLookup({
              ...data,
              consumerPresent: result
              ->SharedLoggerRuntime.anyToJson
              ->JSON.Decode.object
              ->Option.flatMap(object => object->Dict.get("consumerPresent"))
              ->Option.flatMap(JSON.Decode.bool)
              ->Option.getOr(false),
            })->SharedLoggerRuntime.eventDetails
          | _ => []
          },
        ~message?,
        ~call,
      ),
  )

let observeMerchantCall = (
  ~method: merchantMethod,
  ~surface=?,
  ~details=?,
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
        ~sharedDetailsOf=result =>
          switch method {
          | IsCustomerPresent(data) =>
            IsCustomerPresent({
              ...data,
              customerPresent: result
              ->SharedLoggerRuntime.anyToJson
              ->JSON.Decode.object
              ->Option.flatMap(object => object->Dict.get("customerPresent"))
              ->Option.flatMap(JSON.Decode.bool)
              ->Option.getOr(false),
            })->SharedLoggerRuntime.eventDetails
          | _ => []
          },
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
        ~sharedDetailsOf=SharedLoggerRuntime.httpDetails,
        ~message?,
        ~call=() => Fetch.fetch(url, request),
      ),
  )
