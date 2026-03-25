let shouldEmitEvent = (
  ~eventType: PaymentEventTypes.events,
  ~subscribedEvents: array<PaymentEventTypes.events>,
): bool => {
  if subscribedEvents->Array.length === 0 {
    false
  } else {
    subscribedEvents->Array.some(subscribed => subscribed === eventType)
  }
}

type cardInfo = {
  bin: option<string>,
  last4: option<string>,
  brand: option<string>,
  expiryMonth: option<string>,
  expiryYear: option<string>,
  formattedExpiry: option<string>,
  isCardNumberComplete: bool,
  isCvcComplete: bool,
  isExpiryComplete: bool,
  isCardNumberValid: bool,
  isExpiryValid: bool,
}

let buildCardInfo = (
  ~cardNumber: string,
  ~expiry: string,
  ~cvc: string,
  ~brand: string,
): cardInfo => {
  let cleanNumber = cardNumber->Validation.clearSpaces
  let len = cleanNumber->String.length

  let bin = if len >= 6 {
    Some(cleanNumber->String.substring(~start=0, ~end=6))
  } else {
    None
  }

  let isCardNumberComplete = Validation.isCardNumberEqualsMax(cleanNumber, brand)

  let last4 = if isCardNumberComplete {
    Some(cleanNumber->String.substring(~start=len - 4, ~end=len))
  } else {
    None
  }

  let (month, year) = expiry->Validation.getExpiryDates
  let expiryMonth = if month !== "" {
    Some(month)
  } else {
    None
  }
  let expiryYear = if year->String.length == 4 {
    Some(year)
  } else {
    None
  }

  let formattedExpiry = if month !== "" && year->String.length == 4 {
    Some(expiry)
  } else {
    None
  }

  let isCardNumberValid = Validation.cardValid(cardNumber, brand)
  let isExpiryValid = Validation.checkCardExpiry(expiry)

  let isCardNumberComplete = isCardNumberComplete && isCardNumberValid
  let isExpiryComplete = isExpiryValid
  let isCvcComplete = Validation.checkCardCVC(cvc, brand)

  {
    bin,
    last4,
    brand: if brand === "" {
      None
    } else {
      Some(brand)
    },
    expiryMonth,
    expiryYear,
    formattedExpiry,
    isCardNumberComplete,
    isCvcComplete,
    isExpiryComplete,
    isCardNumberValid,
    isExpiryValid,
  }
}

let buildCardInfoFromSavedCard = (
  ~bin: string,
  ~last4: string,
  ~brand: string,
  ~expiryMonth: string,
  ~expiryYear: string,
  ~isCvcComplete: bool,
): cardInfo => {
  let formattedExpiry = `${expiryMonth}/${expiryYear->String.substring(~start=2, ~end=4)}`

  {
    bin: Some(bin),
    last4: Some(last4),
    brand: Some(brand),
    expiryMonth: Some(expiryMonth),
    expiryYear: Some(expiryYear),
    formattedExpiry: Some(formattedExpiry),
    isCardNumberComplete: true,
    isCardNumberValid: true,
    isExpiryComplete: true,
    isExpiryValid: true,
    isCvcComplete,
  }
}

let cardInfoToJson = (info: cardInfo): JSON.t => {
  [
    ("bin", info.bin->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("last4", info.last4->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("brand", info.brand->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("expiryMonth", info.expiryMonth->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("expiryYear", info.expiryYear->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    (
      "formattedExpiry",
      info.formattedExpiry->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null),
    ),
    ("isCardNumberComplete", info.isCardNumberComplete->JSON.Encode.bool),
    ("isCvcComplete", info.isCvcComplete->JSON.Encode.bool),
    ("isExpiryComplete", info.isExpiryComplete->JSON.Encode.bool),
    ("isCardNumberValid", info.isCardNumberValid->JSON.Encode.bool),
    ("isExpiryValid", info.isExpiryValid->JSON.Encode.bool),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

type paymentMethodStatusEvent = {
  paymentMethod: string,
  paymentMethodType: string,
  isSavedPaymentMethod: bool,
  isOneClickWallet: bool,
}

let buildPaymentMethodStatusEvent = (
  ~paymentMethod: string,
  ~paymentMethodType: string,
  ~isSavedPaymentMethod: bool=false,
  ~isOneClickWallet: bool=false,
): paymentMethodStatusEvent => {
  {paymentMethod, paymentMethodType, isSavedPaymentMethod, isOneClickWallet}
}

let paymentMethodStatusEventToJson = (
  ~paymentMethod: string,
  ~paymentMethodType: string,
  ~isSavedPaymentMethod: bool=false,
  ~isOneClickWallet: bool=false,
): JSON.t => {
  [
    ("paymentMethod", paymentMethod->JSON.Encode.string),
    ("isSavedPaymentMethod", isSavedPaymentMethod->JSON.Encode.bool),
    ("paymentMethodType", paymentMethodType->JSON.Encode.string),
    ("isOneClickWallet", isOneClickWallet->JSON.Encode.bool),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

type formStatusEvent = {status: string}

let buildFormStatusEvent = (~status: PaymentEventTypes.formStatusValue): formStatusEvent => {
  {status: PaymentEventTypes.formStatusValueToString(status)}
}

let formStatusEventToJson = (~status: PaymentEventTypes.formStatusValue): JSON.t => {
  [("status", status->PaymentEventTypes.formStatusValueToString->JSON.Encode.string)]
  ->Dict.fromArray
  ->JSON.Encode.object
}

type paymentMethodInfoAddress = {
  country: string,
  state: string,
  postalCode: string,
}

let buildPaymentMethodInfoAddress = (
  ~country: string,
  ~state: string,
  ~postalCode: string,
): paymentMethodInfoAddress => {
  {country, state, postalCode}
}

let paymentMethodInfoAddressToJson = (
  ~country: string,
  ~state: string,
  ~postalCode: string,
): JSON.t => {
  [
    ("country", country->JSON.Encode.string),
    ("state", state->JSON.Encode.string),
    ("postalCode", postalCode->JSON.Encode.string),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

let computeFormStatus = (~isComplete: bool, ~isEmpty: bool): PaymentEventTypes.formStatusValue => {
  if isComplete {
    Complete
  } else if isEmpty {
    Empty
  } else {
    Filling
  }
}

type cvcStatusEvent = {
  requiresCvv: bool,
  isCvcComplete: bool,
  isFocused: bool,
}

let buildCvcStatusEvent = (
  ~requiresCvv: bool,
  ~isCvcComplete: bool=false,
  ~isFocused: bool=false,
): cvcStatusEvent => {
  {requiresCvv, isCvcComplete, isFocused}
}

let cvcStatusEventToJson = (event: cvcStatusEvent): JSON.t => {
  [
    ("requiresCvv", event.requiresCvv->JSON.Encode.bool),
    ("isCvcComplete", event.isCvcComplete->JSON.Encode.bool),
    ("isFocused", event.isFocused->JSON.Encode.bool),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}
