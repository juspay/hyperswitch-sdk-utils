type provider =
  | VisaUctp
  | VisaDirect
  | MastercardUctp
  | MastercardDirect

type providerDetails = {provider: provider}

type cardsData = {provider: provider, actionCode: string, visa: int, mastercard: int}

type declineData = {provider: provider, code: string}

// Lifecycle

type lifecycleEvent =
  | ProviderReady(providerDetails)
  | ProviderUnavailable(providerDetails)
  | RecognitionTokenFound(providerDetails)
  | CardsListed(cardsData)
  | CardsUnavailable(declineData)
  | CustomerVerificationRequired(providerDetails)
  | CustomerRecognised(providerDetails)
  | CheckoutCompleted(providerDetails)
  | CheckoutDeclined(declineData)
  | CheckoutCancelled(declineData)
  | CheckoutFailed(providerDetails)
  | OtpRejected(providerDetails)
  | AuthenticationFailed(providerDetails)
  | PopupBlocked(providerDetails)
  | SignOutFailed(providerDetails)

let lifecycleSeverity = event =>
  switch event {
  | ProviderReady(_)
  | RecognitionTokenFound(_)
  | CheckoutCancelled(_)
  | CardsListed(_) => SharedLoggerTypes.Debug
  | CustomerVerificationRequired(_)
  | CustomerRecognised(_)
  | CheckoutCompleted(_)
  | CheckoutDeclined(_) =>
    Info
  | ProviderUnavailable(_)
  | CardsUnavailable(_)
  | OtpRejected(_)
  | SignOutFailed(_) =>
    Warning
  | CheckoutFailed(_)
  | AuthenticationFailed(_)
  | PopupBlocked(_) =>
    Error
  }

// State and user actions

type stateEvent = ClickToPayViewChanged({view: string})

let stateSeverity = (_: stateEvent) => SharedLoggerTypes.Debug

type verificationSource = ClickToPayOtp | ClickToPayIdentity

type openedView = ClickToPayIdentityChange

type userEvent =
  | CustomerVerificationSubmitted({source: verificationSource, provider: option<provider>})
  | ClickToPayOtpResendRequested
  | ViewOpened({view: openedView})
  | FieldToggled({field: string, enabled: bool})

let userSeverity = event =>
  switch event {
  | CustomerVerificationSubmitted(_)
  | ClickToPayOtpResendRequested
  | ViewOpened(_) => SharedLoggerTypes.Info
  | FieldToggled(_) => SharedLoggerTypes.Debug
  }

// Function

type functionEvent =
  | Initialize(providerDetails)
  | Init(providerDetails)
  | IdentityLookup(providerDetails)
  | GetCards(providerDetails)
  | Authenticate(providerDetails)
  | EncryptCard(providerDetails)
  | Checkout(providerDetails)
  | CheckoutWithCard(providerDetails)
  | CheckoutWithNewCard(providerDetails)
  | UnbindAppInstance(providerDetails)
  | SignOut(providerDetails)

let functionSeverity = event =>
  switch event {
  | Initialize(_)
  | Init(_)
  | IdentityLookup(_)
  | GetCards(_)
  | Authenticate(_)
  | UnbindAppInstance(_)
  | SignOut(_) => SharedLoggerTypes.quietSuccessSoftFailure
  | EncryptCard(_)
  | Checkout(_)
  | CheckoutWithCard(_)
  | CheckoutWithNewCard(_) => SharedLoggerTypes.quietSuccess
  }

// Merchant call

type merchantMethod =
  | InitSession
  | GetActiveSession
  | IsCustomerPresent
  | GetUserType
  | GetRecognizedCards
  | ValidateAuthentication
  | CheckoutWithCard
  | SignOut

let merchantCallSeverity = (method: merchantMethod) =>
  switch method {
  | CheckoutWithCard => SharedLoggerTypes.defaultSeverity
  | InitSession
  | GetActiveSession
  | IsCustomerPresent
  | GetUserType
  | GetRecognizedCards
  | ValidateAuthentication
  | SignOut => SharedLoggerTypes.quietSuccessSoftFailure
  }

// Api

type apiEvent =
  | EnabledAuthnMethodsToken
  | EligibilityCheck
  | AuthenticationSync

let apiSeverity = event =>
  switch event {
  | EnabledAuthnMethodsToken
  | EligibilityCheck
  | AuthenticationSync => SharedLoggerTypes.defaultSeverity
  }

// Resource

type resourceEvent =
  | VisaSdkScript
  | MastercardSdkScript
  | UiKitScript
  | UiKitStylesheet

let resourceSeverity = event =>
  switch event {
  | VisaSdkScript
  | MastercardSdkScript
  | UiKitScript
  | UiKitStylesheet => SharedLoggerTypes.quietSuccessSoftFailure
  }

let isStylesheet = value =>
  switch value {
  | UiKitStylesheet => true
  | VisaSdkScript | MastercardSdkScript | UiKitScript => false
  }
