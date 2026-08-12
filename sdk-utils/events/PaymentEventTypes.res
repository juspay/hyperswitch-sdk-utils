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
  | CardDetailsChange
  | PaymentMethodChange
  | FormStatusChange
  | BillingDetailsChange
  | CvcStatusChange
  | SurchargeInfo
  | UnknownEvent

let eventToString = (eventType: events): string => {
  switch eventType {
  | CardDetailsChange => "cardDetailsChange"
  | PaymentMethodChange => "paymentMethodChange"
  | FormStatusChange => "formStatusChange"
  | BillingDetailsChange => "billingDetailsChange"
  | CvcStatusChange => "cvcStatusChange"
  | SurchargeInfo => "surchargeInfo"
  | UnknownEvent => "UNKNOWN_EVENT"
  }
}

let eventFromString = (str: string): events => {
  switch str {
  | "cardDetailsChange" => CardDetailsChange
  | "paymentMethodChange" => PaymentMethodChange
  | "formStatusChange" => FormStatusChange
  | "billingDetailsChange" => BillingDetailsChange
  | "cvcStatusChange" => CvcStatusChange
  | "surchargeInfo" => SurchargeInfo
  | _ => UnknownEvent
  }
}
