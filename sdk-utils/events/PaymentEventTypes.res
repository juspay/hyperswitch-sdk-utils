type formStatusValue =
  | Empty
  | Filling
  | Complete

let formStatusValueToString = (status: formStatusValue): string => {
  switch status {
  | Empty => "EMPTY"
  | Filling => "FILLING"
  | Complete => "COMPLETE"
  }
}

let formStatusValueFromString = (str: string): formStatusValue => {
  switch str {
  | "EMPTY" => Empty
  | "FILLING" => Filling
  | "COMPLETE" => Complete
  | _ => Empty
  }
}

type events =
  | PaymentMethodInfoCard
  | PaymentMethodStatus
  | FormStatus
  | PaymentMethodInfoBillingAddress
  | UnknownEvent

let eventToString = (eventType: events): string => {
  switch eventType {
  | PaymentMethodInfoCard => "PAYMENT_METHOD_INFO_CARD"
  | PaymentMethodStatus => "PAYMENT_METHOD_STATUS"
  | FormStatus => "FORM_STATUS"
  | PaymentMethodInfoBillingAddress => "PAYMENT_METHOD_INFO_BILLING_ADDRESS"
  | UnknownEvent => "UNKNOWN_EVENT"
  }
}
