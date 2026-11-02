type cardFormScope = PaymentForm | VaultForm
type cardFormData = {scope: cardFormScope}
type cardFieldData = {scope: cardFormScope, field: string}
type cardFormCompletionData = {scope: cardFormScope, fields: array<string>}

type vaultFailure =
  | FieldBindingFailed
  | FieldMountFailed
  | FieldUpdateFailed
  | FieldUnmountFailed
  | FormCreationFailed
  | TokenizationFailed

type vaultFailureData = {reason: vaultFailure}

type stateEvent =
  | CardFormCreated(cardFormData)
  | CardFormDestroyed(cardFormData)
  | CardFieldCreated(cardFieldData)
  | CardFieldMounted(cardFieldData)
  | CardFieldRendered(cardFieldData)
  | CardFormCompleted(cardFormCompletionData)
  | CardFieldUnmounted(cardFieldData)

type paymentMethodSaveOperation = Save | Update
type paymentMethodSaveData = {operation: paymentMethodSaveOperation}
type paymentMethodSaveOutcomeData = {operation: paymentMethodSaveOperation, status: string}

type lifecycleEvent =
  | VaultFlowFailed(vaultFailureData)
  | OperationBlocked({operation: paymentMethodSaveOperation, reason: string})
  | PaymentMethodSaveRejected(paymentMethodSaveData)
  | PaymentMethodSaveSucceeded(paymentMethodSaveOutcomeData)
  | PaymentMethodSaveFailed(paymentMethodSaveOutcomeData)

let stateSeverity = (_: stateEvent) => SharedLoggerTypes.Debug
let lifecycleSeverity = event =>
  switch event {
  | VaultFlowFailed(_) | PaymentMethodSaveRejected(_) => SharedLoggerTypes.Error
  | OperationBlocked(_) => SharedLoggerTypes.Warning
  | PaymentMethodSaveSucceeded(_) | PaymentMethodSaveFailed(_) => SharedLoggerTypes.Info
  }

type functionEvent = VaultFormCreate
type functionCallbackEvent = OnSuccess | OnError
type resourceEvent = VaultScript

type vaultTokenizeScope = SaveCardCvc | FullCard
type vaultTokenizeData = {scope: vaultTokenizeScope}
type apiEvent =
  | VaultTokenization(vaultTokenizeData)
  | PaymentMethodsList
  | RetrievePaymentMethodSession
  | SavePaymentMethod
  | UpdatePaymentMethod
  | DeletePaymentMethod

let functionSeverity = (_: functionEvent) => SharedLoggerTypes.quietSuccess
let functionCallbackSeverity = (_: functionCallbackEvent) => SharedLoggerTypes.quietSuccess
let resourceSeverity = (_: resourceEvent) => SharedLoggerTypes.quietSuccess
let apiSeverity = (_: apiEvent) => SharedLoggerTypes.defaultSeverity
