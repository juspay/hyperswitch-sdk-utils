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
  {
    bin: Some(bin),
    last4: Some(last4),
    brand: Some(brand),
    expiryMonth: Some(expiryMonth),
    expiryYear: Some(expiryYear),
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

let paymentMethodStatusEventToJson = (event: paymentMethodStatusEvent): JSON.t => {
  [
    ("paymentMethod", event.paymentMethod->JSON.Encode.string),
    ("isSavedPaymentMethod", event.isSavedPaymentMethod->JSON.Encode.bool),
    ("paymentMethodType", event.paymentMethodType->JSON.Encode.string),
    ("isOneClickWallet", event.isOneClickWallet->JSON.Encode.bool),
  ]
  ->Dict.fromArray
  ->JSON.Encode.object
}

type formStatusEvent = {status: string}

let buildFormStatusEvent = (~status: PaymentEventTypes.formStatusValue): formStatusEvent => {
  {status: PaymentEventTypes.formStatusValueToString(status)}
}

let formStatusEventToJson = (event: formStatusEvent): JSON.t => {
  [("status", event.status->JSON.Encode.string)]
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

let paymentMethodInfoAddressToJson = (info: paymentMethodInfoAddress): JSON.t => {
  [
    ("country", info.country->JSON.Encode.string),
    ("state", info.state->JSON.Encode.string),
    ("postalCode", info.postalCode->JSON.Encode.string),
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
