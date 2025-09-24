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

type cardPattern = {
  issuer: string,
  pattern: Js.Re.t,
  cvcLength: array<int>,
  length: array<int>,
  maxCVCLength: int,
  pincodeRequired: bool,
}

type validationRule =
  | Required
  | MinLength(int)
  | MaxLength(int)
  | CardNumber
  | CardExpiry
  | CardCVC(string)
  | Email
  | FullName
  | Phone
  | PostalCode(string)
  | IBAN

let defaultCardPattern = {
  issuer: "",
  pattern: %re("/^[0-9]/"),
  cvcLength: [3, 4],
  maxCVCLength: 4,
  length: [13, 14, 15, 16, 17, 18, 19],
  pincodeRequired: false,
}

let cardPatterns = [
  {
    issuer: "Maestro",
    pattern: %re(
      "/^(5018|5081|5044|504681|504993|5020|502260|5038|5893|603845|603123|6304|6759|676[1-3]|6220|504834|504817|504645|504775|600206|627741)/"
    ),
    cvcLength: [3, 4],
    length: [12, 13, 14, 15, 16, 17, 18, 19],
    maxCVCLength: 4,
    pincodeRequired: true,
  },
  {
    issuer: "UnionPay",
    pattern: %re("/^(6[27]|81)/"),
    cvcLength: [3],
    length: [16, 17, 18, 19],
    maxCVCLength: 3,
    pincodeRequired: true,
  },
  {
    issuer: "Interac",
    pattern: %re("/^(4506|4724|4761|0012)/"),
    cvcLength: [3],
    length: [16],
    maxCVCLength: 3,
    pincodeRequired: true,
  },
  {
    issuer: "RuPay",
    pattern: %re(
      "/^(508227|508[5-9]|603741|60698[5-9]|60699|607[0-8]|6079[0-7]|60798[0-4]|60800[1-9]|6080[1-9]|608[1-4]|608500|6521[5-9]|652[2-9]|6530|6531[0-4]|817290|817368|817378|353800|82)/"
    ),
    cvcLength: [3],
    length: [16],
    maxCVCLength: 3,
    pincodeRequired: false,
  },
  {
    issuer: "DinersClub",
    pattern: %re("/^(36|38|39|30[0-5])/"),
    cvcLength: [3],
    maxCVCLength: 3,
    length: [14, 15, 16, 17, 18, 19],
    pincodeRequired: false,
  },
  {
    issuer: "Discover",
    pattern: %re("/^(6011|64[4-9]|65|622126|622[1-9][0-9][0-9]|6229[0-1][0-9]|622925)/"),
    cvcLength: [3],
    length: [16],
    maxCVCLength: 3,
    pincodeRequired: true,
  },
  {
    issuer: "Mastercard",
    pattern: %re("/^(222[1-9]|22[3-9][0-9]|2[3-6][0-9]{2}|27[0-1][0-9]|2720|5[1-5])/"),
    cvcLength: [3],
    maxCVCLength: 3,
    length: [16],
    pincodeRequired: true,
  },
  {
    issuer: "AmericanExpress",
    pattern: %re("/^3[47]/"),
    cvcLength: [4],
    length: [14, 15],
    maxCVCLength: 4,
    pincodeRequired: true,
  },
  {
    issuer: "Visa",
    pattern: %re("/^4/"),
    cvcLength: [3],
    length: [13, 14, 15, 16, 19],
    maxCVCLength: 3,
    pincodeRequired: true,
  },
  {
    issuer: "CartesBancaires",
    pattern: %re(
      "/^(401(005|006|581)|4021(01|02)|403550|405936|406572|41(3849|4819|50(56|59|62|71|74)|6286|65(37|79)|71[7])|420110|423460|43(47(21|22)|50(48|49|50|51|52)|7875|95(09|11|15|39|98)|96(03|18|19|20|22|72))|4424(48|49|50|51|52|57)|448412|4505(19|60)|45(33|56[6-8]|61|62[^3]|6955|7452|7717|93[02379])|46(099|54(76|77)|6258|6575|98[023])|47(4107|71(73|74|86)|72(65|93)|9619)|48(1091|3622|6519)|49(7|83[5-9]|90(0[1-6]|1[0-6]|2[0-3]|3[0-3]|4[0-3]|5[0-2]|68|9[256789]))|5075(89|90|93|94|97)|51(0726|3([0-7]|8[56]|9(00|38))|5214|62(07|36)|72(22|43)|73(65|66)|7502|7647|8101|9920)|52(0993|1662|3718|7429|9227|93(13|14|31)|94(14|21|30|40|47|55|56|[6-9])|9542)|53(0901|10(28|30)|1195|23(4[4-7])|2459|25(09|34|54|56)|3801|41(02|05|11)|50(29|66)|5324|61(07|15)|71(06|12)|8011)|54(2848|5157|9538|98(5[89]))|55(39(79|93)|42(05|60)|4965|7008|88(67|82)|89(29|4[23])|9618|98(09|10))|56(0408|12(0[2-6]|4[134]|5[04678]))|58(17(0[0123457]|15|2[14]|3[16789]|4[0-9]|5[016]|6[269]|7[3789]|8[012467]|9[017])|55(0[2-5]|7[7-9]|8[0-2])))/"
    ),
    cvcLength: [3],
    length: [16, 17, 18, 19],
    maxCVCLength: 3,
    pincodeRequired: true,
  },
  {
    issuer: "SODEXO",
    pattern: %re("/^(637513)/"),
    cvcLength: [3],
    length: [16],
    maxCVCLength: 3,
    pincodeRequired: false,
  },
  {
    issuer: "BAJAJ",
    pattern: %re("/^(203040)/"),
    cvcLength: [3],
    maxCVCLength: 3,
    length: [16],
    pincodeRequired: true,
  },
  {
    issuer: "JCB",
    pattern: %re("/^35(2[89]|[3-8][0-9])/"),
    cvcLength: [3],
    maxCVCLength: 3,
    length: [16],
    pincodeRequired: false,
  },
]

let toInt = val => val->Int.fromString->Option.getOr(0)

let clearSpaces = value => {
  value->String.replaceRegExp(%re("/\D+/g"), "")
}

let clearAlphas = value => {
  value->String.replaceRegExp(%re("/[^\d\s]+/g"), "")
}

let slice = (val, start: int, end: int) => {
  val->String.slice(~start, ~end)
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
  cardPatterns
  ->Array.filter(item => {
    cardBrand === item.issuer
  })
  ->Array.get(0)
  ->Option.getOr(defaultCardPattern)
}

let getAllMatchedCardSchemes = cardNumber => {
  cardPatterns->Array.reduce([], (acc, item) => {
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

    if doesFallInRange(rupayRanges, card) {
      "RUPAY"
    } else if doesFallInRange(masterCardRanges, card) {
      "MASTERCARD"
    } else {
      cardPatterns
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
      if double > 9 {
        let str = double->Int.toString
        let arr = str->String.split("")
        (arr->Array.get(0)->Option.getOr("")->toInt + arr[1]->Option.getOr("")->toInt)->Int.toString
      } else {
        double->Int.toString
      }
    })

  let sumofCheckArr = Array.reduce(checkArr, 0, (acc, val) => acc + val->toInt)
  let sumofUnCheckedArr = Array.reduce(unCheckArr, 0, (acc, val) => acc + val->toInt)
  let totalSum = sumofCheckArr + sumofUnCheckedArr
  mod(totalSum, 10) == 0
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

let checkMaxCardCvv = (cvcNumber, cardBrand) => {
  cvcNumber->String.length > 0 && cvcNumberEqualsMaxLength(cvcNumber, cardBrand)
}

let checkCardCVC = (cvcNumber, cardBrand) => {
  cvcNumber->String.length > 0 && cvcNumberInRange(cvcNumber, cardBrand)
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

let getExpiryDates = val => {
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
    year->toInt > currentYear && year->toInt < 2099 && month->toInt >= 1 && month->toInt <= 12
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

let isValidZip = (~zipCode, ~country) => {
  let _ = country
  let countryObj = CountryStateDataHookTypes.defaultTimeZone
  let postalCode =
    PostalCodes.postalCode
    ->Array.find(item => item.iso == countryObj.country_code)
    ->Option.getOr(PostalCodes.defaultPostalCode)

  let isZipCodeValid = RegExp.test(postalCode.regex->Js.Re.fromString, zipCode)
  zipCode->String.length > 0 && isZipCodeValid
}

let isValidIban = text => {
  let trimmedText = text->String.trim
  let isIbanEmpty = trimmedText->String.length != 0
  isIbanEmpty
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

let validateField = (
  value: string,
  rules: array<validationRule>,
  ~enabledCardSchemes: array<string>,
) => {
  rules->Array.reduce(None, (acc, rule) => {
    switch acc {
    | Some(_) => acc
    | None =>
      switch rule {
      | Required => {
          let trimmedValue = value->String.trim
          if trimmedValue === "" {
            Some("This field is required")
          } else {
            None
          }
        }
      | MinLength(min) =>
        if value->String.length < min {
          Some(`Minimum ${min->Int.toString} characters required`)
        } else {
          None
        }
      | MaxLength(max) =>
        if value->String.length > max {
          Some(`Maximum ${max->Int.toString} characters allowed`)
        } else {
          None
        }
      | CardNumber => {
          let validCardBrand = getFirstValidCardScheme(~cardNumber=value, ~enabledCardSchemes)
          let cardBrand = validCardBrand === "" ? getCardBrand(value) : validCardBrand
          let formattedNumber = formatCardNumber(value, cardType(cardBrand))
          cardValid(formattedNumber, cardBrand) ? None : Some("Enter a valid card number")
        }
      | CardExpiry => checkCardExpiry(value) ? None : Some("Enter a valid expiry date")
      | CardCVC(cardBrand) => checkCardCVC(value, cardBrand) ? None : Some("Enter a valid CVC")
      | Email =>
        switch value->isEmailValid {
        | Some(true) => None
        | Some(false) => Some("Invalid email address")
        | None => None
        }
      | FullName => {
          let trimmedValue = value->String.trim
          if trimmedValue->String.length < 2 {
            Some("Name must be at least 2 characters")
          } else if containsMoreThanTwoDigits(trimmedValue) {
            Some("Name cannot contain more than 2 digits")
          } else {
            None
          }
        }
      | Phone => {
          let cleanPhone = value->clearSpaces
          if cleanPhone->String.length < 10 {
            Some("Enter a valid phone number")
          } else {
            None
          }
        }
      | PostalCode(country) =>
        isValidZip(~zipCode=value, ~country) ? None : Some("Enter a valid postal code")
      | IBAN => isValidIban(value) ? None : Some("Enter a valid IBAN")
      }
    }
  })
}

let createFieldValidator = (validationRule: validationRule, ~enabledCardSchemes: array<string>) => {
  let rules = []
  rules->Array.push(Required)->ignore
  rules->Array.push(validationRule)->ignore

  (value: option<string>) => {
    validateField(value->Option.getOr(""), rules, ~enabledCardSchemes)
  }
}

let createFieldValidatorWithRules = (
  validationRules: array<validationRule>,
  ~enabledCardSchemes: array<string>,
) => {
  (value: option<string>) => {
    validateField(value->Option.getOr(""), validationRules, ~enabledCardSchemes)
  }
}

let createFieldValidatorOptional = (
  validationRule: validationRule,
  ~enabledCardSchemes: array<string>,
) => {
  let rules = []
  rules->Array.push(validationRule)->ignore

  (value: option<string>) => {
    validateField(value->Option.getOr(""), rules, ~enabledCardSchemes)
  }
}

let getKeyboardType = (validationRule: validationRule) => {
  switch validationRule {
  | CardNumber => #numeric
  | CardExpiry => #numeric
  | CardCVC(_) => #numeric
  | Email => #"email-address"
  | Phone => #"phone-pad"
  | _ => #default
  }
}

let getSecureTextEntry = (validationRule: validationRule) => {
  switch validationRule {
  | CardCVC(_) => true
  | _ => false
  }
}

let format = (value: string, validationRule: validationRule) => {
  let cardBrand = getCardBrand(value)
  switch validationRule {
  | CardNumber => {
      let cleanValue = value->formatCardNumber(cardType(cardBrand))
      cleanValue
    }
  | CardExpiry =>
    if value->String.includes("/") || value->String.length <= 5 {
      value->formatCardExpiryNumber
    } else {
      value->String.substring(~start=0, ~end=4)
    }
  | CardCVC(_) => value->formatCVCNumber(cardBrand)
  | _ => value
  }
}

let formatValue: validationRule => (option<string>, string) => option<string> = (
  validationRule: validationRule,
) => {
  (value: option<string>, _name: string) => {
    value->Option.map(value => format(value, validationRule))
  }
}
