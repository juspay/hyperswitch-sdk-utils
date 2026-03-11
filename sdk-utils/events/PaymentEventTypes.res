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

type t =
  | PaymentMethodInfoCard
  | PaymentMethodStatus
  | FormStatus
  | PaymentMethodInfoAddress

let toString = (eventType: t): string => {
  switch eventType {
  | PaymentMethodInfoCard => "PAYMENT_METHOD_INFO_CARD"
  | PaymentMethodStatus => "PAYMENT_METHOD_STATUS"
  | FormStatus => "FORM_STATUS"
  | PaymentMethodInfoAddress => "PAYMENT_METHOD_INFO_ADDRESS"
  }
}
