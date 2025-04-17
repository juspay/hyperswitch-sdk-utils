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

let getCardType = val => {
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

//web only
let getCardStringFromType = val => {
  switch val {
  | VISA => "Visa"
  | MASTERCARD => "Mastercard"
  | AMEX => "AmericanExpress"
  | MAESTRO => "Maestro"
  | DINERSCLUB => "DinersClub"
  | DISCOVER => "Discover"
  | BAJAJ => "BAJAJ"
  | SODEXO => "SODEXO"
  | RUPAY => "RuPay"
  | JCB => "JCB"
  | CARTESBANCAIRES => "CartesBancaires"
  | UNIONPAY => "UnionPay"
  | INTERAC => "Interac"
  | NOTFOUND => "NOTFOUND"
  }
}

let clearSpaces = value => {
  value->String.replaceRegExp(%re("/\D+/g"), "")
}

let toInt = val => val->Int.fromString->Option.getOr(0)

let slice = (val, start: int, end: int) => {
  val->String.slice(~start, ~end)
}

let getStrFromIndex = (arr: array<string>, index) => {
  arr->Array.get(index)->Option.getOr("")
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

let formatCVCNumber = (val, cardType) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardType)
  clearValue->slice(0, obj.maxCVCLength)
}

let formatCardNumber = (val, cardType) => {
  let clearValue = val->clearSpaces
  let formatedCard = switch cardType {
  | AMEX => `${clearValue->slice(0, 4)} ${clearValue->slice(4, 10)} ${clearValue->slice(10, 15)}`
  | DINERSCLUB =>
    `${clearValue->slice(0, 4)} ${clearValue->slice(4, 10)} ${clearValue->slice(10, 14)}`
  | MASTERCARD
  | DISCOVER
  | SODEXO
  | RUPAY
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

    let masterCardRanges = [(222100, 272099), (510000, 559999)]

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
