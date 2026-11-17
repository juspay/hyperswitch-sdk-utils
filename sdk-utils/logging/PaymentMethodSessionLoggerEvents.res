type cardFormScope =
  | @as("VAULT_FORM") VaultForm
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

type vaultFailureData = {
  reason: vaultFailure,
  code?: string,
  field?: string,
  vault?: string,
  operation?: string,
}

type stateEvent =
  | CardFormCreated(cardFormData)
  | CardFormDestroyed(cardFormData)
  | CardFieldCreated(cardFieldData)
  | CardFieldMounted(cardFieldData)
  | CardFieldRendered(cardFieldData)
  | CardFormCompleted(cardFormCompletionData)
  | CardFieldUnmounted(cardFieldData)

type paymentMethodSaveOperation =
  | @as("SAVE") Save
  | @as("UPDATE") Update
type paymentMethodSaveData = {operation: paymentMethodSaveOperation}
type paymentMethodSaveOutcomeData = {operation: paymentMethodSaveOperation, status: string}
type operationBlockedData = {operation: paymentMethodSaveOperation, reason: string}

type lifecycleEvent =
  | VaultFlowFailed(vaultFailureData)
  | OperationBlocked(operationBlockedData)
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

type vaultTokenizeScope =
  | @as("SAVE_CARD_CVC") SaveCardCvc
  | @as("FULL_CARD") FullCard
type vaultTokenizeData = {scope: vaultTokenizeScope, vault?: string}
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
