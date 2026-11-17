open SharedLoggerTypes

// Shared

type provider =
  | @as("VISA_UCTP") VisaUctp
  | @as("VISA_DIRECT") VisaDirect
  | @as("MASTERCARD_UCTP") MastercardUctp
  | @as("MASTERCARD_DIRECT") MastercardDirect

type providerDetails = {provider: provider}
type identityLookupData = {provider: provider, consumerPresent?: bool}
type customerPresenceData = {emailProvided?: bool, customerPresent?: bool}
type checkoutWithCardData = {rememberMe: bool, cardCount: int}

type cardsData = {provider: provider, actionCode: string, visa: int, mastercard: int}

type declineData = {provider: provider, code: string, actionCode?: string}

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
  | SignOutCompleted(providerDetails)
  | SignOutFailed(providerDetails)

let lifecycleSeverity = event =>
  switch event {
  | ProviderReady(_)
  | RecognitionTokenFound(_)
  | CheckoutCancelled(_)
  | CardsListed(_) =>
    Debug
  | SignOutCompleted(_) => Info
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

type viewData = {view: string}

type stateEvent = ClickToPayViewChanged(viewData)

let stateSeverity = (_: stateEvent) => Debug

type verificationSource =
  | @as("CLICK_TO_PAY_OTP") ClickToPayOtp
  | @as("CLICK_TO_PAY_IDENTITY") ClickToPayIdentity

type openedView =
  | @as("CLICK_TO_PAY_IDENTITY_CHANGE") ClickToPayIdentityChange

type verificationData = {source: verificationSource, provider: option<provider>}
type openedViewData = {view: openedView}
type fieldToggleData = {field: string, enabled: bool}

type userEvent =
  | CustomerVerificationSubmitted(verificationData)
  | ClickToPayOtpResendRequested
  | ViewOpened(openedViewData)
  | FieldToggled(fieldToggleData)

let userSeverity = event =>
  switch event {
  | CustomerVerificationSubmitted(_)
  | ClickToPayOtpResendRequested
  | ViewOpened(_) => Info
  | FieldToggled(_) => Debug
  }

// Function

type functionEvent =
  | Initialize(providerDetails)
  | Init(providerDetails)
  | IdentityLookup(identityLookupData)
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
  | SignOut(_) => quietSuccessSoftFailure
  | EncryptCard(_)
  | Checkout(_)
  | CheckoutWithCard(_)
  | CheckoutWithNewCard(_) => quietSuccess
  }

// Merchant call

type merchantMethod =
  | InitSession
  | GetActiveSession
  | IsCustomerPresent(customerPresenceData)
  | GetUserType
  | GetRecognizedCards
  | ValidateAuthentication
  | CheckoutWithCard(checkoutWithCardData)
  | SignOut

let merchantCallSeverity = (method: merchantMethod) =>
  switch method {
  | CheckoutWithCard(_) => defaultSeverity
  | InitSession
  | GetActiveSession
  | IsCustomerPresent(_)
  | GetUserType
  | GetRecognizedCards
  | ValidateAuthentication
  | SignOut => quietSuccessSoftFailure
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
  | AuthenticationSync => defaultSeverity
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
  | UiKitStylesheet => quietSuccessSoftFailure
  }

let isStylesheet = value =>
  switch value {
  | UiKitStylesheet => true
  | VisaSdkScript | MastercardSdkScript | UiKitScript => false
  }
