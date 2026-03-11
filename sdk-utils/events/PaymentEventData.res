let shouldEmitEvent = (~eventType: string, ~subscribedEvents: array<string>): bool => {
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
  isCardNumberComplete: bool,
  isCvcComplete: bool,
  isExpiryComplete: bool,
  isCardNumberValid: bool,
  isExpiryValid: bool,
  isCvcValid: bool,
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
  let isCvcComplete = Validation.cardValid(cvc, brand)

  let last4 = if isCardNumberComplete {
    Some(cleanNumber->String.substring(~start=len - 4, ~end=len))
  } else {
    None
  }

  let (month, year) = expiry->Validation.splitExpiryDates
  let expiryMonth = if month !== "" {
    Some(month)
  } else {
    None
  }
  let expiryYear = if year !== "" {
    Some(year)
  } else {
    None
  }

  let isCardNumberValid = Validation.cardValid(cardNumber, brand)
  let isExpiryValid = Validation.checkCardExpiry(expiry)
  let isCvcValid = Validation.checkCardCVC(cvc, brand)

  let isCardNumberComplete = isCardNumberComplete && isCardNumberValid
  let isExpiryComplete = isExpiryValid
  let isCvcComplete = isCvcComplete

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
    isCardNumberComplete,
    isCvcComplete,
    isExpiryComplete,
    isCardNumberValid,
    isExpiryValid,
    isCvcValid,
  }
}

let cardInfoToJson = (info: cardInfo): JSON.t => {
  [
    ("bin", info.bin->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("last4", info.last4->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("brand", info.brand->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("expiryMonth", info.expiryMonth->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("expiryYear", info.expiryYear->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("isCardNumberComplete", info.isCardNumberComplete->JSON.Encode.bool),
    ("isCvcComplete", info.isCvcComplete->JSON.Encode.bool),
    ("isExpiryComplete", info.isExpiryComplete->JSON.Encode.bool),
    ("isCardNumberValid", info.isCardNumberValid->JSON.Encode.bool),
    ("isExpiryValid", info.isExpiryValid->JSON.Encode.bool),
    ("isCvcValid", info.isCvcValid->JSON.Encode.bool),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

type paymentMethodStatusEvent = {
  paymentMethod: string,
  paymentMethodType: option<string>,
  isSavedPaymentMethod: bool,
  isOneClickWallet: option<bool>,
}

let buildPaymentMethodStatusEvent = (
  ~paymentMethod: string,
  ~paymentMethodType: option<string>=?,
  ~isSavedPaymentMethod: bool=false,
  ~isOneClickWallet: option<bool>=?,
): paymentMethodStatusEvent => {
  {paymentMethod, paymentMethodType, isSavedPaymentMethod, isOneClickWallet}
}

let paymentMethodStatusEventToJson = (event: paymentMethodStatusEvent): JSON.t => {
  [
    ("paymentMethod", event.paymentMethod->JSON.Encode.string),
    ("isSavedPaymentMethod", event.isSavedPaymentMethod->JSON.Encode.bool),
    (
      "paymentMethodType",
      event.paymentMethodType->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null),
    ),
    (
      "isOneClickWallet",
      event.isOneClickWallet->Option.map(JSON.Encode.bool)->Option.getOr(JSON.Null),
    ),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

type formStatusEvent = {
  status: string,
  paymentMethod: option<string>,
}

let buildFormStatusEvent = (
  ~status: PaymentEventTypes.formStatusValue,
  ~paymentMethod: option<string>=?,
): formStatusEvent => {
  {status: PaymentEventTypes.formStatusValueToString(status), paymentMethod}
}

let formStatusEventToJson = (event: formStatusEvent): JSON.t => {
  [
    ("status", event.status->JSON.Encode.string),
    ("paymentMethod", event.paymentMethod->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

type paymentMethodInfoAddress = {
  country: option<string>,
  state: option<string>,
  postalCode: option<string>,
}

let buildPaymentMethodInfoAddress = (
  ~country: option<string>=?,
  ~state: option<string>=?,
  ~postalCode: option<string>=?,
): paymentMethodInfoAddress => {
  {country, state, postalCode}
}

let paymentMethodInfoAddressToJson = (info: paymentMethodInfoAddress): JSON.t => {
  [
    ("country", info.country->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("state", info.state->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
    ("postalCode", info.postalCode->Option.map(JSON.Encode.string)->Option.getOr(JSON.Null)),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

let computeFormStatus = (
  ~hasRequiredFields: bool,
  ~isFormValid: bool,
  ~isPristine: bool,
): PaymentEventTypes.formStatusValue => {
  if !hasRequiredFields {
    PaymentEventTypes.Complete
  } else if isFormValid {
    PaymentEventTypes.Complete
  } else if isPristine {
    PaymentEventTypes.Empty
  } else {
    PaymentEventTypes.Filling
  }
}
