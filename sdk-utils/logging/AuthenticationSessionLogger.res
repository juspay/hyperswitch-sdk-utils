include ClickToPayLoggerEvents

// A standalone authentication session must never inherit a payment identity,
// including when the web queue backfills missing identifiers later.
let context = () => {
  ...SharedLoggerRuntime.platform().getContext(),
  paymentId: "",
  pmSessionId: "",
  scoped: true,
}

let logLifecycle = (~event: lifecycleEvent, ~details=?, ~exn=?, ~message=?) =>
  ClickToPayLogger.logLifecycle(
    ~event,
    ~details?,
    ~exn?,
    ~message?,
    ~context=context(),
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let logState = (~event: stateEvent) =>
  ClickToPayLogger.logState(
    ~event,
    ~context=context(),
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let logUser = (~event: userEvent) =>
  ClickToPayLogger.logUser(
    ~event,
    ~context=context(),
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let observeFunction = (~event: functionEvent, ~detailsOf=?, ~message=?, ~call) =>
  ClickToPayLogger.observeFunction(
    ~event,
    ~detailsOf?,
    ~message?,
    ~call,
    ~context=context(),
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let observeMerchantCall = (~method: merchantMethod, ~details=?, ~detailsOf=?, ~message=?, ~call) =>
  ClickToPayLogger.observeMerchantCall(
    ~method,
    ~details?,
    ~detailsOf?,
    ~message?,
    ~call,
    ~context=context(),
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let observeApi = (~event: apiEvent, ~url, ~message=?, ~call) =>
  ClickToPayLogger.observeApi(
    ~event,
    ~url,
    ~message?,
    ~call,
    ~context=context(),
    ~source=SharedLoggerRuntime.AuthenticationSession,
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
  ClickToPayLogger.observeResource(
    ~event,
    ~url,
    ~attributes?,
    ~matchQuery?,
    ~message?,
    ~onLoad?,
    ~onError?,
    ~context=context(),
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )
