open SharedLoggerTypes

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
  | Deinit(surfaceData)
  | Create(surfaceData)
  | CreateCardForm(surfaceData)
  | GetElement(surfaceData)
  | Mount(surfaceData)
  | Unmount(surfaceData)
  | Destroy(surfaceData)
  | Update(surfaceData)
  | ConfirmPayment(surfaceData)
  | ConfirmWithCustomerDefaultPaymentMethod(surfaceData)
  | ConfirmWithLastUsedPaymentMethod(surfaceData)
  | RetrievePaymentIntent(surfaceData)
  | PaymentRequest(surfaceData)
  | PaymentMethodsManagementElements(surfaceData)
  | GetCustomerSavedPaymentMethods(surfaceData)
  | GetCustomerDefaultSavedPaymentMethodData(surfaceData)
  | GetCustomerLastUsedPaymentMethodData(surfaceData)
  | UpdateIntent(surfaceData)
  | FetchUpdates(surfaceData)
  | Tokenize(surfaceData)

let merchantCallSeverity = event =>
  switch event {
  | ConfirmPayment(_)
  | ConfirmWithCustomerDefaultPaymentMethod(_)
  | ConfirmWithLastUsedPaymentMethod(_)
  | PaymentRequest(_)
  | Tokenize(_) => defaultSeverity
  | Deinit(_)
  | Create(_)
  | CreateCardForm(_)
  | GetElement(_)
  | Mount(_)
  | Unmount(_)
  | Destroy(_)
  | Update(_)
  | RetrievePaymentIntent(_)
  | PaymentMethodsManagementElements(_)
  | GetCustomerSavedPaymentMethods(_)
  | GetCustomerDefaultSavedPaymentMethodData(_)
  | GetCustomerLastUsedPaymentMethodData(_)
  | UpdateIntent(_)
  | FetchUpdates(_) => quietSuccess
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
