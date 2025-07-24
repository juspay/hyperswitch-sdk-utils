let toInt = val => val->Int.fromString->Option.getOr(0) 

let getobjFromCardPattern = cardBrand => {
  let patternsDict = CardPattern.cardPatterns
  patternsDict
  ->Array.filter(item => {
    cardBrand === item.issuer
  })
  ->Array.get(0)
  ->Option.getOr(CardPattern.defaultCardPattern)
}

let clearSpaces = value => {
  value->String.replaceRegExp(%re("/\D+/g"), "") 
}

let slice = (val, start: int, end: int) => {
  val->String.slice(~start, ~end)
}

let getAllMatchedCardSchemes = cardNumber => {
  CardPattern.cardPatterns->Array.reduce([], (acc, item) => {
    if String.match(cardNumber, item.pattern)->Option.isSome {
      acc->Array.push(item.issuer)
    }
    acc
  })
}

let isCardSchemeEnabled = (~cardScheme, ~enabledCardSchemes) => {
  enabledCardSchemes->Array.includes(cardScheme)
}

let formatCVCNumber = (val, cardType) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardType)
  clearValue->slice(0, obj.maxCVCLength)
}

let getStrFromIndex = (arr: array<string>, index) => {
  arr->Array.get(index)->Option.getOr("")
}

let splitExpiryDates = val => {
  let split = val->String.split("/")
  let value = split->Array.map(item => item->String.trim)
  let month = value->Array.get(0)->Option.getOr("")
  let year = value->Array.get(1)->Option.getOr("")
  (month, year)
}

let formatCardExpiryNumber = val => {
  let clearValue = val->clearSpaces
  let expiryVal = clearValue->toInt
  let formatted = if expiryVal >= 2 && expiryVal <= 9 && clearValue->String.length == 1 {
    `0${clearValue} / `
  } else if clearValue->String.length == 2 && expiryVal > 12 {
    let val = clearValue->String.split("")
    `0${val->getStrFromIndex(0)} / ${val->getStrFromIndex(1)}`
  } else {
    clearValue
  }

  if clearValue->String.length >= 3 {
    `${formatted->slice(0, 2)} / ${formatted->slice(2, 4)}` 
  } else {
    formatted
  }
}

let isEmailValid = email => {
  switch email->String.match(
    %re(
      "/^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/"
    ),
  ) {
  | Some(_match) => Some(true)
  | None => email->String.length > 0 ? Some(false) : None
  }
}
