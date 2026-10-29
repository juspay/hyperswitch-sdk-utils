open! SharedLoggerTypes
module Provider = {
  type provider =
    | VisaUctp
    | VisaDirect
    | MastercardUctp
    | MastercardDirect
}
module CommonSdk = {
  type walletFlow =
    | Normal
    | Delayed
    | ThirdParty
    | PaypalSdkTabs

  type walletFailure =
    | MissingIntent
    | MissingCurrency
    | MissingNonce
    | ConnectorUnsupported
    | ClientUnavailable
    | BrowserUnsupported
    | PaymentsNotAllowed
    | ClientCreationFailed
    | AvailabilityCheckFailed
    | ListenerSetupFailed
    | MessageHandlingFailed
    | PaymentDataFailed
    | SdkMountFailed
    | SheetFailed

  type vaultFailure =
    | FieldBindingFailed
    | FieldMountFailed
    | FieldUpdateFailed
    | FieldUnmountFailed
    | FormCreationFailed
    | TokenizationFailed

  type threeDsMethodFailure =
    | MissingContainer
    | FormSubmitFailed
    | IframeLoadFailed

  type threeDsPopupFailure = MessageHandlingFailed

  type ddcFailure =
    | MissingUrl
    | MissingRedirectUrl
    | InvalidNextAction
    | UnreadableResponse

  type walletFlowData = {flow: walletFlow, connector?: string}

  type walletFailureData = {reason: walletFailure, connector?: string}

  type vaultFailureData = {reason: vaultFailure}

  type transStatusData = {transStatus: string}

  type threeDsMethodFailureData = {reason: threeDsMethodFailure}

  type threeDsPopupFailureData = {reason: threeDsPopupFailure}

  type ddcFailureData = {reason: ddcFailure}

  type paymentOutcomeData = {status: string, manualRetryAllowed?: bool}

  type bankAuthSyncFailureData = {status: string}

  type customerRedirectData = {nextAction: string, redirectMode?: string, redirectOrigin: string}

  type nextActionFailureData = {nextAction: string, recovered: bool}

  type unknownPaymentMethodData = {value: string}

  type unsupportedConnectorData = {connector: string}

  type paymentStatusUnknownData = {inferred: bool}

  type validationFailureData = {reason: string}

  type blockedOperation =
    | PaymentConfirm
    | PaymentSync
    | PaymentCompleteAuthorize
    | PaymentPostSessionTokens
    | PaymentMethodSave
    | PaymentMethodUpdate
    | IntentUpdate

  type operationBlockedData = {operation: blockedOperation, reason: string}

  type paymentMethodSaveOperation =
    | Save
    | Update

  type paymentMethodSaveData = {operation: paymentMethodSaveOperation}

  type paymentMethodSaveOutcomeData = {operation: paymentMethodSaveOperation, status: string}

  type retryExhaustionData = {operation: string, attempts: int}

  let walletFailureSeverity = (reason: walletFailure): severity =>
    switch reason {
    | BrowserUnsupported => Debug
    | MissingIntent
    | MissingCurrency
    | ConnectorUnsupported
    | PaymentsNotAllowed
    | ClientCreationFailed
    | AvailabilityCheckFailed
    | ListenerSetupFailed
    | SdkMountFailed =>
      Warning
    | MissingNonce
    | ClientUnavailable
    | MessageHandlingFailed
    | PaymentDataFailed
    | SheetFailed =>
      Error
    }

  let threeDsMethodFailureSeverity = (reason: threeDsMethodFailure): severity =>
    switch reason {
    | MissingContainer | FormSubmitFailed => Error
    | IframeLoadFailed => Warning
    }

  let vaultFailureSeverity = (reason: vaultFailure): severity =>
    switch reason {
    | FieldBindingFailed
    | FieldMountFailed
    | FormCreationFailed
    | FieldUpdateFailed
    | FieldUnmountFailed
    | TokenizationFailed =>
      Error
    }

  type cardFormScope = PaymentForm | VaultForm

  type loaderState = Loading | SemiLoaded | Loaded | LoadFailed

  type networkData = {online: bool}

  type cardFormData = {scope: cardFormScope}

  type cardFieldData = {scope: cardFormScope, field: string}
  type cardFormCompletionData = {scope: cardFormScope, fields: array<string>}

  type loaderSource = IframeMount | PaymentMethodsList | SdkConfigs

  type loaderData = {state: loaderState, source: loaderSource}

  type clickToPayViewData = {view: string}

  type updateIntentData = {inProgress: bool}

  type formCompletionData = {savedMethod: bool}

  type merchantControlData = {control: string}

  type stateEvent =
    | NetworkStatusChanged(networkData)
    | ElementOptionsChanged
    | LoaderStateChanged(loaderData)
    | CardFormCreated(cardFormData)
    | CardFormDestroyed(cardFormData)
    | CardFieldCreated(cardFieldData)
    | CardFieldMounted(cardFieldData)
    | CardFieldRendered(cardFieldData)
    | CardFormCompleted(cardFormCompletionData)
    | CardFieldUnmounted(cardFieldData)
    | CardCoBadgeDetected
    | MerchantControlReceived(merchantControlData)
    | DynamicFieldsChanged
    | PaymentFormCompleted(formCompletionData)
    | UpdateIntentProgressChanged(updateIntentData)
    | ClickToPayViewChanged(clickToPayViewData)

  let stateSeverity = (value: stateEvent): severity =>
    switch value {
    | LoaderStateChanged({state: LoadFailed}) => Error
    | LoaderStateChanged({state: Loaded}) => Info
    | NetworkStatusChanged({online}) => online ? Debug : Warning
    | LoaderStateChanged(_)
    | ElementOptionsChanged
    | CardFormCreated(_)
    | CardFormDestroyed(_)
    | CardFieldCreated(_)
    | CardFieldMounted(_)
    | CardFieldRendered(_)
    | CardFormCompleted(_)
    | CardFieldUnmounted(_)
    | CardCoBadgeDetected
    | MerchantControlReceived(_)
    | DynamicFieldsChanged
    | PaymentFormCompleted(_)
    | UpdateIntentProgressChanged(_)
    | ClickToPayViewChanged(_) =>
      Debug
    }

  // User

  type view =
    | SavedMethodList
    | MorePaymentMethods
    | InstallmentOptions

  type openedView =
    | NewPaymentMethods
    | ManageSavedMethod
    | ClickToPayIdentityChange
    | CardSchemeMenu

  type submitSource =
    | PayButton
    | SaveCardButton
    | MerchantApi

  type verificationSource =
    | ClickToPayOtp
    | ClickToPayIdentity

  type fieldData = {field: string}

  type fieldToggleData = {field: string, enabled: bool}

  type methodData = {method: string}

  type savedMethodSelectionData = {requiresCvv: bool, isCardExpired: bool}

  type viewData = {view: view, expanded: bool}

  type openedViewData = {view: openedView}

  type vaultTokenizeScope =
    | SaveCardCvc
    | FullCard

  type vaultTokenizeData = {scope: vaultTokenizeScope}

  type functionCallbackEvent =
    | OnValidateMerchant
    | OnPaymentAuthorized
    | OnShippingContactSelected
    | OnCancel
    | OnPaymentDataChanged
    | CreateOrder
    | CreateBillingAgreement
    | OnApprove
    | OnShippingAddressChange
    | OnError
    | OnClick
    | OnLoad
    | OnSuccess
    | OnExit

  let functionCallbackSeverity = (value: functionCallbackEvent) =>
    switch value {
    | OnShippingContactSelected
    | OnShippingAddressChange
    | OnPaymentDataChanged
    | OnClick
    | OnCancel
    | OnExit
    | OnLoad => quietSuccessSoftFailure
    | OnValidateMerchant
    | OnPaymentAuthorized
    | CreateOrder
    | CreateBillingAgreement
    | OnApprove
    | OnSuccess
    | OnError => quietSuccess
    }

  // Resource

  type resourceEvent =
    | GooglePayScript
    | SamsungPayScript
    | ApplePayScript
    | PaypalScript
    | PazeScript
    | KlarnaScript
    | TrustpayScript
    | BraintreeClientScript
    | BraintreeApplePayScript
    | PmAuthConnectorScript
    | VaultScript
    | FontStylesheet

  let resourceSeverity = (value: resourceEvent) =>
    switch value {
    | VaultScript => quietSuccess
    | GooglePayScript
    | SamsungPayScript
    | ApplePayScript
    | PaypalScript
    | PazeScript
    | KlarnaScript
    | TrustpayScript
    | BraintreeClientScript
    | BraintreeApplePayScript
    | PmAuthConnectorScript
    | FontStylesheet => quietSuccessSoftFailure
    }

  // Static asset

  type crashOrigin =
    | ErrorBoundary
    | UncaughtError
    | UnhandledRejection
    | EntryPoint
    | ElementConstructor
    | ParentWindowMessage
    | MerchantCallback

  let crashSeverity = (origin: crashOrigin): severity =>
    switch origin {
    | ErrorBoundary
    | UncaughtError
    | UnhandledRejection
    | EntryPoint
    | ElementConstructor
    | ParentWindowMessage
    | MerchantCallback =>
      Error
    }
}
module NativeSdk = {
  include CommonSdk
  type walletStage =
    | ConfirmRequestReceived
    | OneClickDeclined
    | SheetStarted

  type walletStageData = {stage: walletStage, connector?: string}

  type sdkClosedData = {status: string}

  type idleData = {idleMs: int}

  // Funnel milestones are INFO so business metrics survive a non-debug log level.
  type sdkInitData = {credentialType: string}
  type sdkRenderData = {startupApiCount: int, degraded: bool}
  type firstInteractionData = {interaction: string}
  type confirmAttemptData = {attemptId: string}
  type confirmResponseData = {attemptId: string, outcome: string, status?: string, statusCode?: int}
  type authenticationStartedData = {operation: string}
  type paymentDetailsData = {savedMethod: bool}

  type lifecycleEvent =
    | ElementIframeMounted
    | ElementInitReceived
    | AppRendered
    | PaymentAttempted
    | PaymentSucceeded(paymentOutcomeData)
    | PaymentFailed(paymentOutcomeData)
    | PaymentRejected
    | PaymentStatusUnknown(paymentStatusUnknownData)
    | PaymentRetriesExhausted(retryExhaustionData)
    | PaymentErrorHandlingFailed
    | PaymentStatusPollExhausted
    | PaymentMethodSaveRejected(paymentMethodSaveData)
    | PaymentMethodSaveSucceeded(paymentMethodSaveOutcomeData)
    | PaymentMethodSaveFailed(paymentMethodSaveOutcomeData)
    | OperationBlocked(operationBlockedData)
    | FormValidationFailed(validationFailureData)
    | WalletFlowResolved(walletFlowData)
    | WalletStageReached(walletStageData)
    | WalletFlowFailed(walletFailureData)
    | WalletFlowExited
    | WalletTokenReceived
    | VaultFlowFailed(vaultFailureData)
    | BankAuthSyncFailed(bankAuthSyncFailureData)
    | BankAuthConnectorUnsupported(unsupportedConnectorData)
    | CustomerRedirectStarted(customerRedirectData)
    | NextActionUnsupported(nextActionFailureData)
    | ThreeDsPopupRequested
    | ThreeDsPopupFailed(threeDsPopupFailureData)
    | ThreeDsChallengeShown(transStatusData)
    | ThreeDsFrictionlessResolved(transStatusData)
    | ThreeDsAuthContainerMissing(transStatusData)
    | ThreeDsAuthRequestFailed
    | ThreeDsMethodStarted
    | ThreeDsMethodCompleted
    | ThreeDsMethodSkipped
    | ThreeDsMethodFailed(threeDsMethodFailureData)
    | ThreeDsMethodTimedOut
    | DdcStarted
    | DdcCompleted
    | DdcFailed(ddcFailureData)
    | DdcTimedOut
    | QrCodeShown
    | QrCodeExpired
    | VoucherShown
    | BankTransferShown
    | PaymentMethodUnresolved(unknownPaymentMethodData)
    | CountryDataServedFromBundle
    | CountryDataUnavailable
    | EligibilityCheckCancelled
    | EligibilityCheckFailed
    // Mobile funnel milestones (existing web-compatible events remain unchanged).
    | SdkInit(sdkInitData)
    | SdkMounted
    | SdkShimmerShown
    | SdkRendered(sdkRenderData)
    | FirstUserInteraction(firstInteractionData)
    | PaymentConfirmAttempted(confirmAttemptData)
    | PaymentConfirmResponse(confirmResponseData)
    | PaymentConfirmFailed(confirmAttemptData)
    | PaymentConfirmTimedOut(confirmAttemptData)
    | ThreeDsStarted
    | AuthenticationStarted(authenticationStartedData)
    | PaymentDetailsFilled(paymentDetailsData)
    // Mobile only
    | ThreeDsSdkUnavailable
    | SdkClosed(sdkClosedData)
    | CustomerWentIdle(idleData)

  let lifecycleSeverity = (value: lifecycleEvent): severity =>
    switch value {
    | DdcStarted => SharedLoggerConfig.Native.ddcStartedSeverity
    | ElementIframeMounted
    | WalletFlowResolved(_)
    | WalletStageReached(_)
    | ThreeDsPopupRequested
    | ThreeDsMethodStarted
    | ThreeDsMethodCompleted
    | ThreeDsMethodSkipped
    | DdcCompleted
    | CountryDataServedFromBundle
    | EligibilityCheckCancelled =>
      Debug
    | SdkInit(_)
    | SdkMounted
    | SdkShimmerShown
    | SdkRendered(_)
    | FirstUserInteraction(_)
    | PaymentConfirmAttempted(_)
    | PaymentConfirmResponse(_)
    | ThreeDsStarted
    | AuthenticationStarted(_)
    | PaymentDetailsFilled(_)
    | SdkClosed(_)
    | CustomerWentIdle(_) =>
      Info
    | PaymentConfirmFailed(_) => Error
    | PaymentConfirmTimedOut(_) => Warning
    | ThreeDsSdkUnavailable => Warning
    | ElementInitReceived
    | AppRendered
    | PaymentAttempted
    | PaymentSucceeded(_)
    | PaymentFailed(_)
    | PaymentMethodSaveSucceeded(_)
    | PaymentMethodSaveFailed(_)
    | WalletTokenReceived
    | CustomerRedirectStarted(_)
    | ThreeDsChallengeShown(_)
    | ThreeDsFrictionlessResolved(_)
    | QrCodeShown
    | VoucherShown
    | BankTransferShown =>
      Info
    | ThreeDsMethodTimedOut
    | DdcTimedOut
    | BankAuthConnectorUnsupported(_)
    | PaymentMethodUnresolved(_)
    | PaymentStatusUnknown(_)
    | WalletFlowExited
    | QrCodeExpired
    | OperationBlocked(_)
    | PaymentStatusPollExhausted
    | FormValidationFailed(_)
    | EligibilityCheckFailed =>
      Warning
    | PaymentRetriesExhausted(_) => Error
    | PaymentErrorHandlingFailed
    | PaymentMethodSaveRejected(_)
    | PaymentRejected
    | ThreeDsAuthContainerMissing(_)
    | ThreeDsAuthRequestFailed
    | BankAuthSyncFailed(_)
    | CountryDataUnavailable
    | DdcFailed(_) =>
      Error
    | ThreeDsPopupFailed(_) => Error
    | NextActionUnsupported({recovered}) => recovered ? Warning : Error
    | VaultFlowFailed({reason}) => reason->vaultFailureSeverity
    | WalletFlowFailed({reason}) => reason->walletFailureSeverity
    | ThreeDsMethodFailed({reason}) => reason->threeDsMethodFailureSeverity
    }

  // State

  type submitData = {source: submitSource}

  // Same provider values as web; native does not depend on its Click to Pay logger.
  type provider = VisaUctp | VisaDirect | MastercardUctp | MastercardDirect

  type verificationData = {
    source: verificationSource,
    provider: option<provider>,
  }

  type userEvent =
    | PaymentMethodSelected(methodData)
    | CardSchemeSelected(methodData)
    | SavedMethodSelected(savedMethodSelectionData)
    | SavedMethodUpdateRequested
    | SavedMethodDeleteRequested
    | PaymentSubmitted(submitData)
    | CustomerVerificationSubmitted(verificationData)
    | BankDetailsConfirmed
    | ExpressCheckoutClicked
    | VoucherDownloadRequested
    | QrCodeCopyRequested
    | ThreeDsPopupDismissed
    | ClickToPayOtpResendRequested
    | FieldToggled(fieldToggleData)
    | FieldFocused(fieldData)
    | FieldBlurred(fieldData)
    | ViewToggled(viewData)
    | ViewOpened(openedViewData)
    // Mobile only
    | CardScanRequested

  let userSeverity = (value: userEvent): severity =>
    switch value {
    | PaymentMethodSelected(_)
    | CardSchemeSelected(_)
    | SavedMethodSelected(_)
    | SavedMethodUpdateRequested
    | SavedMethodDeleteRequested
    | PaymentSubmitted(_)
    | CustomerVerificationSubmitted(_)
    | ExpressCheckoutClicked
    | ThreeDsPopupDismissed
    | VoucherDownloadRequested
    | QrCodeCopyRequested
    | BankDetailsConfirmed
    | ClickToPayOtpResendRequested
    | CardScanRequested =>
      Info
    | FieldToggled(_)
    | FieldFocused(_)
    | FieldBlurred(_)
    | ViewToggled(_)
    | ViewOpened(_) =>
      Debug
    }

  // Api

  type apiEvent =
    | RetrievePaymentIntent
    | ConfirmCall
    | ConfirmPayoutCall
    | CompleteAuthorize
    | PostSessionTokens
    | Sessions
    | Authentication
    | PollStatus
    | PaymentMethodsList
    | CreateCustomerPaymentMethods
    | RetrievePaymentMethodSession
    | SavePaymentMethod
    | UpdatePaymentMethod
    | DeletePaymentMethod
    | PaymentMethodEligibility
    | PaymentMethodsAuthLink
    | PaymentMethodsAuthExchange
    | TaxCalculation
    | ClientList
    | VaultTokenization(vaultTokenizeData)
    // Mobile only
    | ThreeDsAuthorize

  let apiSeverity = (value: apiEvent) =>
    switch value {
    | Sessions
    | TaxCalculation
    | PaymentMethodEligibility
    | PollStatus => softFailure
    | RetrievePaymentIntent
    | ConfirmCall
    | ConfirmPayoutCall
    | CompleteAuthorize
    | PostSessionTokens
    | Authentication
    | PaymentMethodsList
    | CreateCustomerPaymentMethods
    | RetrievePaymentMethodSession
    | SavePaymentMethod
    | UpdatePaymentMethod
    | DeletePaymentMethod
    | PaymentMethodsAuthLink
    | PaymentMethodsAuthExchange
    | ClientList
    | VaultTokenization(_)
    | ThreeDsAuthorize => defaultSeverity
    }

  // Function

  type functionEvent =
    | LoadPaymentSheet
    | LoadPaymentData
    | IsReadyToPay
    | FinishApplePaymentV2
    | ExecuteGooglePayment
    | BraintreeClientCreate
    | BraintreeApplePayCreate
    | KlarnaInit
    | KlarnaLoad
    | PaypalButtonsRender
    | PlaidCreate
    | VaultFormCreate
    // Mobile only
    | LaunchApplePay
    | LaunchGooglePay
    | LaunchPaypal
    | LaunchSamsungPay
    | LaunchScanCard
    | InitialiseNetcetera
    | GenerateAreqParams
    | ReceiveChallengeParams
    | GenerateChallenge

  let functionSeverity = (value: functionEvent) =>
    switch value {
    | IsReadyToPay | LaunchScanCard => quietSuccessSoftFailure
    | LoadPaymentSheet
    | LoadPaymentData
    | FinishApplePaymentV2
    | ExecuteGooglePayment
    | BraintreeClientCreate
    | BraintreeApplePayCreate
    | KlarnaInit
    | KlarnaLoad
    | PaypalButtonsRender
    | PlaidCreate
    | VaultFormCreate
    | LaunchApplePay
    | LaunchGooglePay
    | LaunchPaypal
    | LaunchSamsungPay
    | InitialiseNetcetera
    | GenerateAreqParams
    | ReceiveChallengeParams
    | GenerateChallenge => quietSuccess
    }

  // Function callback

  type staticAssetEvent =
    | CountryStateData
    | CountryStateDataFallback
    | SdkConfigs
    // Mobile only
    | LocaleStrings

  let staticAssetSeverity = (value: staticAssetEvent) =>
    switch value {
    | SdkConfigs => quietSuccess
    | CountryStateData | LocaleStrings => quietAll
    | CountryStateDataFallback => quietSuccessSoftFailure
    }

  // Crash
}
module WebSdk = {
  include CommonSdk
  type walletStage =
    | ConfirmRequestReceived
    | OneClickDeclined

  type walletStageData = {stage: walletStage, connector?: string}

  type lifecycleEvent =
    | ElementIframeMounted
    | ElementInitReceived
    | AppRendered
    | PaymentAttempted
    | PaymentSucceeded(paymentOutcomeData)
    | PaymentFailed(paymentOutcomeData)
    | PaymentRejected
    | PaymentStatusUnknown(paymentStatusUnknownData)
    | PaymentRetriesExhausted(retryExhaustionData)
    | PaymentErrorHandlingFailed
    | PaymentStatusPollExhausted
    | PaymentMethodSaveRejected(paymentMethodSaveData)
    | PaymentMethodSaveSucceeded(paymentMethodSaveOutcomeData)
    | PaymentMethodSaveFailed(paymentMethodSaveOutcomeData)
    | OperationBlocked(operationBlockedData)
    | FormValidationFailed(validationFailureData)
    | WalletFlowResolved(walletFlowData)
    | WalletStageReached(walletStageData)
    | WalletFlowFailed(walletFailureData)
    | WalletFlowExited
    | WalletTokenReceived
    | VaultFlowFailed(vaultFailureData)
    | BankAuthSyncFailed(bankAuthSyncFailureData)
    | BankAuthConnectorUnsupported(unsupportedConnectorData)
    | CustomerRedirectStarted(customerRedirectData)
    | NextActionUnsupported(nextActionFailureData)
    | ThreeDsPopupRequested
    | ThreeDsPopupFailed(threeDsPopupFailureData)
    | ThreeDsChallengeShown(transStatusData)
    | ThreeDsFrictionlessResolved(transStatusData)
    | ThreeDsAuthContainerMissing(transStatusData)
    | ThreeDsAuthRequestFailed
    | ThreeDsMethodStarted
    | ThreeDsMethodCompleted
    | ThreeDsMethodSkipped
    | ThreeDsMethodFailed(threeDsMethodFailureData)
    | ThreeDsMethodTimedOut
    | DdcStarted
    | DdcCompleted
    | DdcFailed(ddcFailureData)
    | DdcTimedOut
    | QrCodeShown
    | QrCodeExpired
    | VoucherShown
    | BankTransferShown
    | PaymentMethodUnresolved(unknownPaymentMethodData)
    | CountryDataServedFromBundle
    | CountryDataUnavailable
    | EligibilityCheckCancelled
    | EligibilityCheckFailed

  let lifecycleSeverity = value =>
    switch value {
    | DdcStarted => SharedLoggerConfig.Web.ddcStartedSeverity
    | ElementIframeMounted
    | WalletFlowResolved(_)
    | WalletStageReached(_)
    | ThreeDsPopupRequested
    | ThreeDsMethodStarted
    | ThreeDsMethodCompleted
    | ThreeDsMethodSkipped
    | DdcCompleted
    | CountryDataServedFromBundle
    | EligibilityCheckCancelled =>
      Debug
    | ElementInitReceived
    | AppRendered
    | PaymentAttempted
    | PaymentSucceeded(_)
    | PaymentFailed(_)
    | PaymentMethodSaveSucceeded(_)
    | PaymentMethodSaveFailed(_)
    | WalletTokenReceived
    | CustomerRedirectStarted(_)
    | ThreeDsChallengeShown(_)
    | ThreeDsFrictionlessResolved(_)
    | QrCodeShown
    | VoucherShown
    | BankTransferShown =>
      Info
    | ThreeDsMethodTimedOut
    | DdcTimedOut
    | BankAuthConnectorUnsupported(_)
    | PaymentMethodUnresolved(_)
    | PaymentStatusUnknown(_)
    | WalletFlowExited
    | QrCodeExpired
    | OperationBlocked(_)
    | PaymentStatusPollExhausted
    | FormValidationFailed(_)
    | EligibilityCheckFailed =>
      Warning
    | PaymentRetriesExhausted(_) => Error
    | PaymentErrorHandlingFailed
    | PaymentMethodSaveRejected(_)
    | PaymentRejected
    | ThreeDsAuthContainerMissing(_)
    | ThreeDsAuthRequestFailed
    | BankAuthSyncFailed(_)
    | CountryDataUnavailable
    | DdcFailed(_) =>
      Error
    | ThreeDsPopupFailed(_) => Error
    | NextActionUnsupported({recovered}) => recovered ? Warning : Error
    | VaultFlowFailed({reason}) => reason->vaultFailureSeverity
    | WalletFlowFailed({reason}) => reason->walletFailureSeverity
    | ThreeDsMethodFailed({reason}) => reason->threeDsMethodFailureSeverity
    }

  // State

  type submitData = {source: submitSource}

  type verificationData = {
    source: verificationSource,
    provider: option<Provider.provider>,
  }

  type userEvent =
    | PaymentMethodSelected(methodData)
    | CardSchemeSelected(methodData)
    | SavedMethodSelected(savedMethodSelectionData)
    | SavedMethodUpdateRequested
    | SavedMethodDeleteRequested
    | PaymentSubmitted(submitData)
    | CustomerVerificationSubmitted(verificationData)
    | BankDetailsConfirmed
    | ExpressCheckoutClicked
    | VoucherDownloadRequested
    | QrCodeCopyRequested
    | ThreeDsPopupDismissed
    | ClickToPayOtpResendRequested
    | FieldToggled(fieldToggleData)
    | FieldFocused(fieldData)
    | FieldBlurred(fieldData)
    | ViewToggled(viewData)
    | ViewOpened(openedViewData)

  let userSeverity = value =>
    switch value {
    | PaymentMethodSelected(_)
    | CardSchemeSelected(_)
    | SavedMethodSelected(_)
    | SavedMethodUpdateRequested
    | SavedMethodDeleteRequested
    | PaymentSubmitted(_)
    | CustomerVerificationSubmitted(_)
    | ExpressCheckoutClicked
    | ThreeDsPopupDismissed
    | VoucherDownloadRequested
    | QrCodeCopyRequested
    | BankDetailsConfirmed
    | ClickToPayOtpResendRequested =>
      Info
    | FieldToggled(_)
    | FieldFocused(_)
    | FieldBlurred(_)
    | ViewToggled(_)
    | ViewOpened(_) =>
      Debug
    }

  // Api

  type apiEvent =
    | RetrievePaymentIntent
    | ConfirmCall
    | ConfirmPayoutCall
    | CompleteAuthorize
    | PostSessionTokens
    | Sessions
    | Authentication
    | PollStatus
    | PaymentMethodsList
    | CreateCustomerPaymentMethods
    | RetrievePaymentMethodSession
    | SavePaymentMethod
    | UpdatePaymentMethod
    | DeletePaymentMethod
    | PaymentMethodEligibility
    | PaymentMethodsAuthLink
    | PaymentMethodsAuthExchange
    | TaxCalculation
    | ClientList
    | VaultTokenization(vaultTokenizeData)

  let apiSeverity = value =>
    switch value {
    | Sessions
    | TaxCalculation
    | PaymentMethodEligibility
    | PollStatus => softFailure
    | RetrievePaymentIntent
    | ConfirmCall
    | ConfirmPayoutCall
    | CompleteAuthorize
    | PostSessionTokens
    | Authentication
    | PaymentMethodsList
    | CreateCustomerPaymentMethods
    | RetrievePaymentMethodSession
    | SavePaymentMethod
    | UpdatePaymentMethod
    | DeletePaymentMethod
    | PaymentMethodsAuthLink
    | PaymentMethodsAuthExchange
    | ClientList
    | VaultTokenization(_) => defaultSeverity
    }

  // Function

  type functionEvent =
    | LoadPaymentSheet
    | LoadPaymentData
    | IsReadyToPay
    | FinishApplePaymentV2
    | ExecuteGooglePayment
    | BraintreeClientCreate
    | BraintreeApplePayCreate
    | KlarnaInit
    | KlarnaLoad
    | PaypalButtonsRender
    | PlaidCreate
    | VaultFormCreate

  let functionSeverity = value =>
    switch value {
    | IsReadyToPay => quietSuccessSoftFailure
    | LoadPaymentSheet
    | LoadPaymentData
    | FinishApplePaymentV2
    | ExecuteGooglePayment
    | BraintreeClientCreate
    | BraintreeApplePayCreate
    | KlarnaInit
    | KlarnaLoad
    | PaypalButtonsRender
    | PlaidCreate
    | VaultFormCreate => quietSuccess
    }

  // Function callback

  let resourceKind = (value, ~script, ~stylesheet) =>
    switch value {
    | FontStylesheet => stylesheet
    | GooglePayScript
    | SamsungPayScript
    | ApplePayScript
    | PaypalScript
    | PazeScript
    | KlarnaScript
    | TrustpayScript
    | BraintreeClientScript
    | BraintreeApplePayScript
    | PmAuthConnectorScript
    | VaultScript => script
    }

  // Static asset

  type staticAssetEvent =
    | CountryStateData
    | CountryStateDataFallback
    | SdkConfigs

  let staticAssetSeverity = value =>
    switch value {
    | SdkConfigs => quietSuccess
    | CountryStateData => quietAll
    | CountryStateDataFallback => quietSuccessSoftFailure
    }

  // Crash
}
module NativeMerchant = {
  // Surface

  type surface =
    | Hyper
    | Elements
    | PaymentElement
    | CardForm
    | CardField
    | PaymentSession
    | PaymentMethodsSession
    | PaymentMethodsManagement
    // Mobile only
    | PaymentSheet
    | Widget

  type surfaceData = {surface: surface}

  // Merchant call

  type merchantCallEvent =
    | Init(surfaceData)
    | Reinit(surfaceData)
    | Deinit(surfaceData)
    | LoadHyper(surfaceData)
    | CreateElements(surfaceData)
    | CreateWidgets(surfaceData)
    | Create(surfaceData)
    | CreateCardForm(surfaceData)
    | GetElement(surfaceData)
    | Mount(surfaceData)
    | Unmount(surfaceData)
    | Destroy(surfaceData)
    | Update(surfaceData)
    | ConfirmPayment(surfaceData)
    | ConfirmCardPayment(surfaceData)
    | ConfirmOneClickPayment(surfaceData)
    | ConfirmWithCustomerDefaultPaymentMethod(surfaceData)
    | ConfirmWithLastUsedPaymentMethod(surfaceData)
    | RetrievePaymentIntent(surfaceData)
    | PaymentRequest(surfaceData)
    | InitPaymentSession(surfaceData)
    | InitPaymentMethodSession(surfaceData)
    | InitAuthenticationSession(surfaceData)
    | PaymentMethodsManagementElements(surfaceData)
    | GetCustomerSavedPaymentMethods(surfaceData)
    | GetCustomerDefaultSavedPaymentMethodData(surfaceData)
    | GetCustomerLastUsedPaymentMethodData(surfaceData)
    | UpdateIntent(surfaceData)
    | InitiateUpdateIntent(surfaceData)
    | CompleteUpdateIntent(surfaceData)
    | FetchUpdates(surfaceData)
    | Tokenize(surfaceData)
    | ConfirmTokenization(surfaceData)

  let merchantCallSeverity = event =>
    switch event {
    | ConfirmPayment(_)
    | ConfirmCardPayment(_)
    | ConfirmOneClickPayment(_)
    | ConfirmWithCustomerDefaultPaymentMethod(_)
    | ConfirmWithLastUsedPaymentMethod(_)
    | PaymentRequest(_)
    | Tokenize(_)
    | ConfirmTokenization(_) => defaultSeverity
    | Init(_)
    | Reinit(_)
    | Deinit(_)
    | LoadHyper(_)
    | CreateElements(_)
    | CreateWidgets(_)
    | Create(_)
    | CreateCardForm(_)
    | GetElement(_)
    | Mount(_)
    | Unmount(_)
    | Destroy(_)
    | Update(_)
    | RetrievePaymentIntent(_)
    | InitPaymentSession(_)
    | InitPaymentMethodSession(_)
    | InitAuthenticationSession(_)
    | PaymentMethodsManagementElements(_)
    | GetCustomerSavedPaymentMethods(_)
    | GetCustomerDefaultSavedPaymentMethodData(_)
    | GetCustomerLastUsedPaymentMethodData(_)
    | UpdateIntent(_)
    | InitiateUpdateIntent(_)
    | CompleteUpdateIntent(_)
    | FetchUpdates(_) => quietSuccess
    }

  // Merchant prop

  type merchantPropEvent =
    | Appearance(surfaceData)
    | Locale(surfaceData)
    | Loader(surfaceData)
    | Fonts(surfaceData)
    | PaymentElementOptions(surfaceData)
    | PreloadSdkWithParams(surfaceData)
    | TestMode(surfaceData)
    | BlockConfirm(surfaceData)
    | CustomPodUri(surfaceData)
    | CustomBackendUrl(surfaceData)
    | RedirectionFlags(surfaceData)

  let merchantPropSeverity = (event): severity =>
    switch event {
    | Appearance(_)
    | Locale(_)
    | Loader(_)
    | Fonts(_)
    | PaymentElementOptions(_)
    | PreloadSdkWithParams(_)
    | TestMode(_)
    | BlockConfirm(_)
    | CustomPodUri(_)
    | CustomBackendUrl(_)
    | RedirectionFlags(_) =>
      Debug
    }

  // Merchant callback

  type merchantCallbackEvent =
    | OnSdkHandleClick(surfaceData)
    | UpdateIntent(surfaceData)

  let merchantCallbackSeverity = event =>
    switch event {
    | OnSdkHandleClick(_)
    | UpdateIntent(_) => quietSuccessSoftFailure
    }

  // Merchant issue

  type merchantIssue =
    | InvalidPublishableKey
    | InsecureProtocol
    | MissingParameter
    | MalformedValue
    | ImmutableAfterMount
    | CalledAfterSessionEnd
    | ExpectedBoolean
    | ExpectedString
    | ExpectedNumber
    | ValueOutOfRange
    | DynamicFieldsConfig
    | ConnectorMisconfigured
    | UnsupportedOptionValue
    | UnknownOptionKey
    | DeprecatedMethod

  let merchantIssueSeverity = (issue): severity =>
    switch issue {
    | InvalidPublishableKey
    | InsecureProtocol
    | MissingParameter
    | MalformedValue =>
      Error
    | ImmutableAfterMount
    | CalledAfterSessionEnd
    | ExpectedBoolean
    | ExpectedString
    | ExpectedNumber
    | ValueOutOfRange
    | DynamicFieldsConfig
    | ConnectorMisconfigured
    | UnsupportedOptionValue
    | UnknownOptionKey
    | DeprecatedMethod =>
      Warning
    }
}
module WebMerchant = {
  // Surface

  type surface =
    | Hyper
    | Elements
    | PaymentElement
    | CardForm
    | CardField
    | PaymentSession
    | PaymentMethodsSession
    | PaymentMethodsManagement

  type surfaceData = {surface: surface}

  // Merchant call

  type merchantCallEvent =
    | Init(surfaceData)
    | Reinit(surfaceData)
    | Deinit(surfaceData)
    | LoadHyper(surfaceData)
    | CreateElements(surfaceData)
    | CreateWidgets(surfaceData)
    | Create(surfaceData)
    | CreateCardForm(surfaceData)
    | GetElement(surfaceData)
    | Mount(surfaceData)
    | Unmount(surfaceData)
    | Destroy(surfaceData)
    | Update(surfaceData)
    | ConfirmPayment(surfaceData)
    | ConfirmCardPayment(surfaceData)
    | ConfirmOneClickPayment(surfaceData)
    | ConfirmWithCustomerDefaultPaymentMethod(surfaceData)
    | ConfirmWithLastUsedPaymentMethod(surfaceData)
    | RetrievePaymentIntent(surfaceData)
    | PaymentRequest(surfaceData)
    | InitPaymentSession(surfaceData)
    | InitPaymentMethodSession(surfaceData)
    | InitAuthenticationSession(surfaceData)
    | PaymentMethodsManagementElements(surfaceData)
    | GetCustomerSavedPaymentMethods(surfaceData)
    | GetCustomerDefaultSavedPaymentMethodData(surfaceData)
    | GetCustomerLastUsedPaymentMethodData(surfaceData)
    | UpdateIntent(surfaceData)
    | InitiateUpdateIntent(surfaceData)
    | CompleteUpdateIntent(surfaceData)
    | FetchUpdates(surfaceData)
    | Tokenize(surfaceData)
    | ConfirmTokenization(surfaceData)

  let merchantCallSeverity = event =>
    switch event {
    | ConfirmPayment(_)
    | ConfirmCardPayment(_)
    | ConfirmOneClickPayment(_)
    | ConfirmWithCustomerDefaultPaymentMethod(_)
    | ConfirmWithLastUsedPaymentMethod(_)
    | PaymentRequest(_)
    | Tokenize(_)
    | ConfirmTokenization(_) => defaultSeverity
    | Init(_)
    | Reinit(_)
    | Deinit(_)
    | LoadHyper(_)
    | CreateElements(_)
    | CreateWidgets(_)
    | Create(_)
    | CreateCardForm(_)
    | GetElement(_)
    | Mount(_)
    | Unmount(_)
    | Destroy(_)
    | Update(_)
    | RetrievePaymentIntent(_)
    | InitPaymentSession(_)
    | InitPaymentMethodSession(_)
    | InitAuthenticationSession(_)
    | PaymentMethodsManagementElements(_)
    | GetCustomerSavedPaymentMethods(_)
    | GetCustomerDefaultSavedPaymentMethodData(_)
    | GetCustomerLastUsedPaymentMethodData(_)
    | UpdateIntent(_)
    | InitiateUpdateIntent(_)
    | CompleteUpdateIntent(_)
    | FetchUpdates(_) => quietSuccess
    }

  // Merchant prop

  type merchantPropEvent =
    | Appearance(surfaceData)
    | Locale(surfaceData)
    | Loader(surfaceData)
    | Fonts(surfaceData)
    | PaymentElementOptions(surfaceData)
    | PreloadSdkWithParams(surfaceData)
    | TestMode(surfaceData)
    | BlockConfirm(surfaceData)
    | CustomPodUri(surfaceData)
    | CustomBackendUrl(surfaceData)
    | RedirectionFlags(surfaceData)

  let merchantPropSeverity = event =>
    switch event {
    | Appearance(_)
    | Locale(_)
    | Loader(_)
    | Fonts(_)
    | PaymentElementOptions(_)
    | PreloadSdkWithParams(_)
    | TestMode(_)
    | BlockConfirm(_)
    | CustomPodUri(_)
    | CustomBackendUrl(_)
    | RedirectionFlags(_) =>
      Debug
    }

  // Merchant callback

  type merchantCallbackEvent =
    | OnSdkHandleClick(surfaceData)
    | UpdateIntent(surfaceData)

  let merchantCallbackSeverity = event =>
    switch event {
    | OnSdkHandleClick(_)
    | UpdateIntent(_) => quietSuccessSoftFailure
    }

  // Merchant issue

  type merchantIssue =
    | InvalidPublishableKey
    | InsecureProtocol
    | MissingParameter
    | MalformedValue
    | ImmutableAfterMount
    | CalledAfterSessionEnd
    | ExpectedBoolean
    | ExpectedString
    | ExpectedNumber
    | ValueOutOfRange
    | ConnectorMisconfigured
    | DynamicFieldsConfig
    | UnsupportedOptionValue
    | UnknownOptionKey
    | DeprecatedMethod

  let merchantIssueSeverity = (issue): severity =>
    switch issue {
    | InvalidPublishableKey
    | InsecureProtocol
    | MissingParameter
    | MalformedValue =>
      Error
    | ImmutableAfterMount
    | CalledAfterSessionEnd
    | ExpectedBoolean
    | ExpectedString
    | ExpectedNumber
    | ValueOutOfRange
    | ConnectorMisconfigured
    | DynamicFieldsConfig
    | UnsupportedOptionValue
    | UnknownOptionKey
    | DeprecatedMethod =>
      Warning
    }
}
module WebClickToPay = {
  // Shared

  include Provider
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
    | CardsListed(_) =>
      Debug
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
    | IsCustomerPresent
    | GetUserType
    | GetRecognizedCards
    | ValidateAuthentication
    | CheckoutWithCard
    | SignOut

  let merchantCallSeverity = (method: merchantMethod) =>
    switch method {
    | CheckoutWithCard => defaultSeverity
    | InitSession
    | GetActiveSession
    | IsCustomerPresent
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

  let resourceKind = (value, ~script, ~stylesheet) =>
    switch value {
    | UiKitStylesheet => stylesheet
    | VisaSdkScript | MastercardSdkScript | UiKitScript => script
    }
}
