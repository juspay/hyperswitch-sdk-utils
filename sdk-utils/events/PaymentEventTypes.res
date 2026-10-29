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
  | PaymentFlowCancel
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
  | PaymentFlowCancel => "paymentFlowCancel"
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
  | "paymentFlowCancel" => PaymentFlowCancel
  | _ => UnknownEvent
  }
}

// The step of the payment flow the shopper cancelled. Only Sheet happens
// before confirm; every other step happens after the payment is confirmed.
type paymentFlowStep =
  | Sheet
  | DelayedSheet
  | ThreeDsChallenge
  | QrCode

let paymentFlowStepToString = (step: paymentFlowStep): string => {
  switch step {
  | Sheet => "sheet"
  | DelayedSheet => "delayedSheet"
  | ThreeDsChallenge => "threeDsChallenge"
  | QrCode => "qrCode"
  }
}
