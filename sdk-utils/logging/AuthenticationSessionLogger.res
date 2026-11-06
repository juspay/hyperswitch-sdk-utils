include ClickToPayLoggerEvents

// A standalone authentication session must never inherit a payment identity,
// including when the web queue backfills missing identifiers later.
let defaultContext = () => {
  ...SharedLoggerRuntime.platform().getContext(),
  paymentId: "",
  pmSessionId: "",
  scoped: true,
}

let authenticationContext = (context: option<SharedLoggerTypes.context>) =>
  switch context {
  | Some(context) => {...context, paymentId: "", pmSessionId: "", scoped: true}
  | None => defaultContext()
  }

let logLifecycle = (~event: lifecycleEvent, ~details=?, ~exn=?, ~message=?, ~context=?) =>
  ClickToPayLogger.logLifecycle(
    ~event,
    ~details?,
    ~exn?,
    ~message?,
    ~context=context->authenticationContext,
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let logState = (~event: stateEvent, ~context=?) =>
  ClickToPayLogger.logState(
    ~event,
    ~context=context->authenticationContext,
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let logUser = (~event: userEvent, ~context=?) =>
  ClickToPayLogger.logUser(
    ~event,
    ~context=context->authenticationContext,
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let observeFunction = (~event: functionEvent, ~detailsOf=?, ~message=?, ~context=?, ~call) =>
  ClickToPayLogger.observeFunction(
    ~event,
    ~detailsOf?,
    ~message?,
    ~call,
    ~context=context->authenticationContext,
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let observeMerchantCall = (~method: merchantMethod, ~details=?, ~detailsOf=?, ~message=?, ~context=?, ~call) =>
  ClickToPayLogger.observeMerchantCall(
    ~method,
    ~surface="AUTHENTICATION_SESSION",
    ~details?,
    ~detailsOf?,
    ~message?,
    ~call,
    ~context=context->authenticationContext,
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let observeApi = (~event: apiEvent, ~url, ~request: Fetch.Request.init={}, ~message=?, ~context=?) =>
  ClickToPayLogger.observeApi(
    ~event,
    ~url,
    ~message?,
    ~request,
    ~context=context->authenticationContext,
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )

let observeResource = (
  ~event: resourceEvent,
  ~url,
  ~attributes=?,
  ~matchQuery=?,
  ~message=?,
  ~context=?,
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
    ~context=context->authenticationContext,
    ~source=SharedLoggerRuntime.AuthenticationSession,
  )
