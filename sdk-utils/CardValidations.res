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

let getCardBrand = cardNumber => {                        // need to check if we dont return uppercase strings in mbl doesnt cause any issue
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

    let masterCardRanges = [(222100, 272099), (510000, 559999)]

    let doesFallInRange = (cardRanges, isin) => {
      let intIsin =
        isin
        ->String.replaceRegExp(%re("/[^\d]/g"), "")
        ->String.substring(~start=0, ~end=6)
        ->Int.fromString
        ->Option.getOr(0)

      let range = cardRanges->Array.map(cardRange => {
        let (min, max) = cardRange
        intIsin >= min && intIsin <= max
      })
      range->Array.includes(true)
    }

    let patternsDict = CardPattern.cardPatterns
    if doesFallInRange(rupayRanges, card) {
      "RuPay"
    } else if doesFallInRange(masterCardRanges, card) {
      "Mastercard"
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

let calculateLuhn = value => {
  let card = value->clearSpaces
  let splitArr = card->String.split("")
  splitArr->Array.reverse
  let unCheckArr = splitArr->Array.filterWithIndex((_, i) => {
    mod(i, 2) == 0
  })
  let checkArr =
    splitArr
    ->Array.filterWithIndex((_, i) => {
      mod(i + 1, 2) == 0
    })
    ->Array.map(item => {
      let val = item->toInt
      let double = val * 2
      let str = double->Int.toString
      let arr = str->String.split("")

      switch (arr[0], arr[1]) {
      | (Some(first), Some(second)) if double > 9 => (first->toInt + second->toInt)->Int.toString
      | _ => str
      }
    })

  let sumofCheckArr = Array.reduce(checkArr, 0, (acc, val) => acc + val->toInt)
  let sumofUnCheckedArr = Array.reduce(unCheckArr, 0, (acc, val) => acc + val->toInt)
  let totalSum = sumofCheckArr + sumofUnCheckedArr

  mod(totalSum, 10) == 0 || ["3000100811111072", "4000100511112003"]->Array.includes(card) // test cards
}

let maxCardLength = cardBrand => {
  let obj = getobjFromCardPattern(cardBrand)
  Array.reduce(obj.length, 0, (acc, val) => Math.Int.max(acc, val))
}

let isCardLengthValid = (cardBrand, cardNumberLength) => {
  let obj = getobjFromCardPattern(cardBrand)
  Array.includes(obj.length, cardNumberLength)
}

let cardValid = (cardNumber, cardBrand) => {
  let clearValueLength = cardNumber->clearSpaces->String.length
  isCardLengthValid(cardBrand, clearValueLength) && calculateLuhn(cardNumber)
}

let cvcNumberInRange = (val, cardBrand) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardBrand)
  let cvcLengthInRange = obj.cvcLength->Array.map(item => {
    clearValue->String.length == item
  })
  cvcLengthInRange
}

let checkCardCVC = (cvcNumber, cardBrand) => {
  cvcNumber->String.length > 0 && cvcNumberInRange(cvcNumber, cardBrand)->Array.includes(true)
}

// Shared Card Number Formatting Function (used in both web and mobile)
let formatCardNumber = (val, cardType) => {
  let clearValue = val->clearSpaces
  let formatedCard = switch cardType {
  | "AmericanExpress" =>
    `${clearValue->slice(0, 4)} ${clearValue->slice(4, 10)} ${clearValue->slice(10, 15)}`
  | "DinersClub" =>
    if clearValue->String.length > 14 {
      `${clearValue->slice(0, 4)} ${clearValue->slice(4, 8)} ${clearValue->slice(
          8,
          12,
        )} ${clearValue->slice(12, 16)} ${clearValue->slice(16, 19)}`
    } else {
      `${clearValue->slice(0, 4)} ${clearValue->slice(4, 10)} ${clearValue->slice(10, 14)}`
    }
  | "Mastercard"
  | "Discover"
  | "SODEXO"
  | "RuPay"
  | "UnionPay"
  | "Visa" =>
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

// Shared Card Expiry Validation Functions (used in both web and mobile)
let getCurrentMonthAndYear = (dateTimeIsoString: string) => {
  let tempTimeDateString = dateTimeIsoString->String.replace("Z", "")
  let tempTimeDate = tempTimeDateString->String.split("T")

  let date = tempTimeDate->Array.get(0)->Option.getOr("")         // only this line is changed therefore no risk at all
  let dateComponents = date->String.split("-")

  let currentMonth = dateComponents->Array.get(1)->Option.getOr("")
  let currentYear = dateComponents->Array.get(0)->Option.getOr("")

  (currentMonth->toInt, currentYear->toInt)
}

let getExpiryDates = val => {           //same
  let date = Date.make()->Date.toISOString
  let (month, year) = splitExpiryDates(val)
  let (_, currentYear) = getCurrentMonthAndYear(date)
  let prefix = currentYear->Int.toString->String.slice(~start=0, ~end=2)
  (month, `${prefix}${year}`)
}

let getExpiryValidity = cardExpiry => {         
  let date = Date.make()->Date.toISOString
  let (month, year) = getExpiryDates(cardExpiry)
  let (currentMonth, currentYear) = getCurrentMonthAndYear(date)
  let valid = if currentYear == year->toInt && month->toInt >= currentMonth && month->toInt <= 12 {
    true
  } else if (
    year->toInt > currentYear &&
    year->toInt < Date.getFullYear(Js.Date.fromFloat(Date.now())) + 100 &&              // in mobile earlier it was hard coded as 2099 but we are using web approach which is current year + 100
    month->toInt >= 1 &&
    month->toInt <= 12
  ) {
    true
  } else {
    false
  }
  valid
}

let checkCardExpiry = expiry => {
  expiry->String.length > 0 && getExpiryValidity(expiry)
}

let isExpiryComplete = val => {
  let (month, year) = splitExpiryDates(val)
  month->String.length == 2 && year->String.length == 2               // Not found in mbl
}

let isExipryValid = val => {                  // Not found in mbl
  val->String.length > 0 && getExpiryValidity(val) && isExpiryComplete(val)
}
