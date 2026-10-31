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
  | AppliedOffersInfo
  | PaymentMethodFlowCancel
  | UnknownEvent

let eventToString = (eventType: events): string => {
  switch eventType {
  | CardDetailsChange => "cardDetailsChange"
  | PaymentMethodChange => "paymentMethodChange"
  | FormStatusChange => "formStatusChange"
  | BillingDetailsChange => "billingDetailsChange"
  | CvcStatusChange => "cvcStatusChange"
  | SurchargeInfo => "surchargeInfo"
  | AppliedOffersInfo => "appliedOffersInfo"
  | PaymentMethodFlowCancel => "paymentMethodFlowCancel"
  | UnknownEvent => "unknownEvent"
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
  | "appliedOffersInfo" => AppliedOffersInfo
  | "paymentMethodFlowCancel" => PaymentMethodFlowCancel
  | _ => UnknownEvent
  }
}

// The screen the shopper closed when cancelling a payment method flow.
type paymentMethodFlowStep =
  | PaymentSheet
  | ThreeDsChallenge

let paymentMethodFlowStepToString = (step: paymentMethodFlowStep): string => {
  switch step {
  | PaymentSheet => "paymentSheet"
  | ThreeDsChallenge => "threeDsChallenge"
  }
}
