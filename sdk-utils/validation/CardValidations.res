type cardIssuer =
  | VISA
  | MASTERCARD
  | AMEX
  | MAESTRO
  | DINERSCLUB
  | DISCOVER
  | BAJAJ
  | SODEXO
  | RUPAY
  | JCB
  | CARTESBANCAIRES
  | UNIONPAY
  | INTERAC
  | NOTFOUND

let toInt = val => val->Int.fromString->Option.getOr(0)

let cardType = val => {
  switch val->String.toUpperCase {
  | "VISA" => VISA
  | "MASTERCARD" => MASTERCARD
  | "AMEX" => AMEX
  | "MAESTRO" => MAESTRO
  | "DINERSCLUB" => DINERSCLUB
  | "DISCOVER" => DISCOVER
  | "BAJAJ" => BAJAJ
  | "SODEXO" => SODEXO
  | "RUPAY" => RUPAY
  | "JCB" => JCB
  | "CARTESBANCAIRES" => CARTESBANCAIRES
  | "UNIONPAY" => UNIONPAY
  | "INTERAC" => INTERAC
  | _ => NOTFOUND
  }
}

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

let calculateLuhn = value => {
  let card = value->clearSpaces
  let length = card->String.length

  if length == 0 {
    false
  } else {
    let rec luhnSum = (index, sum, shouldDouble) => {
      if index < 0 {
        sum
      } else {
        let digit = card->String.get(index)->Option.getOr("0")->toInt
        let processedDigit = if shouldDouble {
          let doubled = digit * 2
          doubled > 9 ? doubled - 9 : doubled
        } else {
          digit
        }
        luhnSum(index - 1, sum + processedDigit, !shouldDouble)
      }
    }

    let totalSum = luhnSum(length - 1, 0, false)
    mod(totalSum, 10) == 0
  }
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

let getFirstValidCardScheme = (~cardNumber, ~enabledCardSchemes) => {
  let allMatchedCards = getAllMatchedCardSchemes(cardNumber->clearSpaces)
  allMatchedCards
  ->Array.find(card => isCardSchemeEnabled(~cardScheme=card, ~enabledCardSchemes))
  ->Option.getOr("")
}

let getEligibleCoBadgedCardSchemes = (~matchedCardSchemes, ~enabledCardSchemes) => {
  matchedCardSchemes->Array.filter(ele => enabledCardSchemes->Array.includes(ele))
}

let getCardBrand = cardNumber => {
  try {
    let card = cardNumber->String.replaceRegExp(%re("/[^\d]/g"), "")
    let rupayRanges = [
      (508227, 508227),
      (508500, 508999),
      (603741, 603741),
      (606985, 607384),
      (607385, 607484),
      (607485, 607984),
      (608001, 608100),
      (608101, 608200),
      (608201, 608300),
      (608301, 608350),
      (608351, 608500),
      (652150, 652849),
      (652850, 653049),
      (653050, 653149),
      (817290, 817290),
      (817368, 817368),
      (817378, 817378),
      (353800, 353800),
    ]

    let masterCardRanges = [(222100, 272099)]

    let doesFallInRange = (cardRanges, isin) => {
      let intIsin =
        isin
        ->String.replaceRegExp(%re("/[^\d]/g"), "")
        ->String.substring(~start=0, ~end=6)
        ->Int.fromString
        ->Option.getOr(0)

      let range = cardRanges->Array.map(currCardRange => {
        let (min, max) = currCardRange

        intIsin >= min && intIsin <= max
      })
      range->Array.includes(true)
    }
    let patternsDict = CardPattern.cardPatterns
    if doesFallInRange(rupayRanges, card) {
      "RUPAY"
    } else if doesFallInRange(masterCardRanges, card) {
      "MASTERCARD"
    } else {
      patternsDict
      ->Array.map(item => {
        if String.match(card, item.pattern)->Option.isSome {
          item.issuer
        } else {
          ""
        }
      })
      ->Array.filter(item => item !== "")
      ->Array.get(0)
      ->Option.getOr("")
    }
  } catch {
  | _error => ""
  }
}

let maxCardLength = cardBrand => {
  let obj = getobjFromCardPattern(cardBrand)
  Array.reduce(obj.length, 0, (acc, val) => acc > val ? acc : val)
}

let cardValid = (cardNumber, cardBrand) => {
  let clearValue = cardNumber->clearSpaces
  Array.includes(getobjFromCardPattern(cardBrand).length, clearValue->String.length) &&
  calculateLuhn(cardNumber)
}

let isCardNumberEqualsMax = (cardNumber, cardBrand) => {
  let clearValue = cardNumber->clearSpaces
  clearValue->String.length == maxCardLength(cardBrand) || clearValue->String.length == 16
}

let formatCardNumber = (val, cardType) => {
  let clearValue = val->clearSpaces
  let formatedCard = switch cardType {
  | AMEX => `${clearValue->slice(0, 4)} ${clearValue->slice(4, 10)} ${clearValue->slice(10, 15)}`
  | DINERSCLUB =>
    if clearValue->String.length > 14 {
      `${clearValue->slice(0, 4)} ${clearValue->slice(4, 8)} ${clearValue->slice(
          8,
          12,
        )} ${clearValue->slice(12, 16)}   ${clearValue->slice(16, 19)}`
    } else {
      `${clearValue->slice(0, 4)} ${clearValue->slice(4, 10)} ${clearValue->slice(10, 14)}`
    }
  | MASTERCARD
  | DISCOVER
  | SODEXO
  | RUPAY
  | UNIONPAY
  | VISA =>
    `${clearValue->slice(0, 4)} ${clearValue->slice(4, 8)} ${clearValue->slice(
        8,
        12,
      )} ${clearValue->slice(12, 16)} ${clearValue->slice(16, 19)}`
  | _ =>
    `${clearValue->slice(0, 4)} ${clearValue->slice(4, 8)} ${clearValue->slice(
        8,
        12,
      )} ${clearValue->slice(12, 19)}`
  }

  formatedCard->String.trim
}

let formatCVCNumber = (val, cardType) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardType)
  clearValue->slice(0, obj.maxCVCLength)
}

let cvcNumberInRange = (val, cardBrand) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardBrand)
  let cvcLengthInRange =
    obj.cvcLength
    ->Array.find(item => {
      clearValue->String.length == item
    })
    ->Option.isSome
  cvcLengthInRange
}

let cvcNumberEqualsMaxLength = (val, cardBrand) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardBrand)
  let cvcMaxLengthEquals = clearValue->String.length == obj.maxCVCLength
  cvcMaxLengthEquals
}

let checkCardCVC = (cvcNumber, cardBrand) => {
  cvcNumber->String.length > 0 && cvcNumberInRange(cvcNumber, cardBrand)
}

let checkMaxCardCvv = (cvcNumber, cardBrand) => {
  cvcNumber->String.length > 0 && cvcNumberEqualsMaxLength(cvcNumber, cardBrand)
}

let getStrFromIndex = (arr: array<string>, index) => {
  arr->Array.get(index)->Option.getOr("")
}

let getCurrentMonthAndYear = (dateTimeIsoString: string) => {
  let tempTimeDateString = dateTimeIsoString->String.replace("Z", "")
  let tempTimeDate = tempTimeDateString->String.split("T")

  let date = tempTimeDate->Array.get(0)->Option.getOr("")
  let dateComponents = date->String.split("-")

  let currentMonth = dateComponents->Array.get(1)->Option.getOr("")
  let currentYear = dateComponents->Array.get(0)->Option.getOr("")

  (currentMonth->toInt, currentYear->toInt)
}

let splitExpiryDates = val => {
  let split = val->String.split("/")
  let value = split->Array.map(item => item->String.trim)
  let month = value->Array.get(0)->Option.getOr("")
  let year = value->Array.get(1)->Option.getOr("")
  (month, year)
}

let getExpiryDates = val => {
  let date = Date.make()->Date.toISOString
  let (month, year) = splitExpiryDates(val)
  let (_, currentYear) = getCurrentMonthAndYear(date)
  let prefix = currentYear->Int.toString->String.slice(~start=0, ~end=2)
  (month, `${prefix}${year}`)
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

let getExpiryValidity = cardExpiry => {
  let date = Date.make()->Date.toISOString
  let (month, year) = getExpiryDates(cardExpiry)
  let (currentMonth, currentYear) = getCurrentMonthAndYear(date)
  let valid = if currentYear == year->toInt && month->toInt >= currentMonth && month->toInt <= 12 {
    true
  } else if (
    year->toInt > currentYear && year->toInt < 2099 && month->toInt >= 1 && month->toInt <= 12
  ) {
    true
  } else {
    false
  }
  valid
}

let isExpiryComplete = val => {
  let (month, year) = splitExpiryDates(val)
  month->String.length == 2 && year->String.length == 2
}

let checkCardExpiry = expiry => {
  expiry->String.length > 0 && getExpiryValidity(expiry)
}

let isExipryValid = val => {
  val->String.length > 0 && getExpiryValidity(val) && isExpiryComplete(val)
}

let containsOnlyDigits = text => {
  %re("/^[0-9]*$/")->Js.Re.test_(text)
}

let containsDigit = text => {
  switch text->String.match(%re("/\d/")) {
  | Some(_) => true
  | None => false
  }
}

let containsMoreThanTwoDigits = text => {
  switch text->String.match(%re("/\d/g")) {
  | Some(matches) => matches->Array.length > 2
  | None => false
  }
}

let isValidIban = text => {
  let trimmedText = text->String.trim
  let isIbanEmpty = trimmedText->String.length != 0
  isIbanEmpty
}

let isValidZip = (~zipCode, ~country) => {
  let _ = country
  zipCode->String.length > 0
}
