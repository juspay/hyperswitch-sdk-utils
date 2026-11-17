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

type fieldData = {surface: surface, field?: string, vault?: string}
type containerData = {
  surface: surface,
  field?: string,
  componentType?: string,
  vault?: string,
}
type updateData = {
  surface: surface,
  field?: string,
  vault?: string,
  fieldCount?: int,
  componentType?: string,
}
type confirmData = {surface: surface, source?: string}
type foundData = {surface: surface, found: bool}
type elementLookupData = {surface: surface, found?: bool}
type implementedData = {surface: surface, implemented?: bool}
type supportedData = {surface: surface, supported?: bool}
type vaultData = {surface: surface, vault?: string}
type testModeData = {surface: surface, testMode?: bool}

type listenerInventoryData = {componentType: string, eventListeners: array<string>}

type merchantStateEvent = PaymentElementEventListeners(listenerInventoryData)

// Merchant call

type merchantCallEvent =
  | Init(surfaceData)
  | Reinit(surfaceData)
  | Deinit(surfaceData)
  | LoadHyper(surfaceData)
  | CreateElements(surfaceData)
  | CreateWidgets(surfaceData)
  | Create(fieldData)
  | CreateCardForm(vaultData)
  | GetElement(elementLookupData)
  | Mount(containerData)
  | Unmount(containerData)
  | Destroy(containerData)
  | Update(updateData)
  | ConfirmPayment(confirmData)
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
  | GetCustomerDefaultSavedPaymentMethodData(foundData)
  | GetCustomerLastUsedPaymentMethodData(foundData)
  | UpdateIntent(supportedData)
  | InitiateUpdateIntent(surfaceData)
  | CompleteUpdateIntent(surfaceData)
  | FetchUpdates(implementedData)
  | Tokenize(vaultData)
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
  | TestMode(testModeData)
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
  | MalformedValue
  | ExpectedBoolean
  | ExpectedString
  | ExpectedNumber =>
    Error
  | ImmutableAfterMount
  | CalledAfterSessionEnd
  | ValueOutOfRange
  | ConnectorMisconfigured
  | DynamicFieldsConfig
  | UnsupportedOptionValue
  | UnknownOptionKey
  | DeprecatedMethod =>
    Warning
  }
