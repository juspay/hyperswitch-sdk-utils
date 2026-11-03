type identity = {
  sessionId: string,
  merchantId: string,
  paymentId: string,
  authenticationId: string,
  pmSessionId: string,
  scoped: bool,
}
type metadata = {
  component: string,
  platformName: string,
  browserName: string,
  browserVersion: string,
  pageHref: string,
  userAgent: string,
}

module NativeTypes = {
  type context = {
    sdkInitAt: option<float>,
    attemptId: string,
    sessionId: string,
    merchantId: string,
    paymentId: string,
    authenticationId: string,
  }
}

module WebTypes = {
  type context = {
    sessionId: string,
    merchantId: string,
    paymentId: string,
    pmSessionId: string,
    authenticationId: string,
    // Independent flows (e.g. vault sessions) never adopt the shared payment identity.
    scoped: bool,
  }
}

let nativeIdentifiers = (context: NativeTypes.context): identity => {
  sessionId: context.sessionId,
  merchantId: context.merchantId,
  paymentId: context.paymentId,
  authenticationId: context.authenticationId,
  pmSessionId: "",
  scoped: false,
}
let webIdentifiers = (context: WebTypes.context): identity => {
  sessionId: context.sessionId,
  merchantId: context.merchantId,
  paymentId: context.paymentId,
  authenticationId: context.authenticationId,
  pmSessionId: context.pmSessionId,
  scoped: context.scoped,
}
