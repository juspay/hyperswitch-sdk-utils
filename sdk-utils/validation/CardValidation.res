/**
 * Hyperswitch Shared Card Validation Library
 * 
 * This module provides comprehensive card validation utilities shared across
 * Hyperswitch client SDKs (mobile and web). It includes functions for:
 * - Card number validation using Luhn algorithm
 * - Card brand detection and formatting
 * - CVC validation
 * - Expiry date validation
 * - Card scheme matching and filtering
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */

// Card issuer enumeration - supports all major card brands
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

// Helper function to convert string to int safely
let toInt = val => val->Int.fromString->Option.getOr(0)

// Convert string to cardIssuer enum
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

// Get card pattern object from CardPattern module
let getobjFromCardPattern = cardBrand => {
  let patternsDict = CardPattern.cardPatterns
  patternsDict
  ->Array.filter(item => {
    cardBrand === item.issuer
  })
  ->Array.get(0)
  ->Option.getOr(CardPattern.defaultCardPattern)
}

// Remove all non-digit characters
let clearSpaces = value => {
  value->String.replaceRegExp(%re("/\D+/g"), "")
}

// String slicing utility
let slice = (val, start: int, end: int) => {
  val->String.slice(~start, ~end)
}

// Optimized Luhn algorithm for card number validation
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

// Get all card schemes that match a card number
let getAllMatchedCardSchemes = cardNumber => {
  CardPattern.cardPatterns->Array.reduce([], (acc, item) => {
    if String.match(cardNumber, item.pattern)->Option.isSome {
      acc->Array.push(item.issuer)
    }
    acc
  })
}

// Check if a card scheme is enabled
let isCardSchemeEnabled = (~cardScheme, ~enabledCardSchemes) => {
  enabledCardSchemes->Array.includes(cardScheme)
}

// Get first valid card scheme from enabled schemes
let getFirstValidCardScheme = (~cardNumber, ~enabledCardSchemes) => {
  let allMatchedCards = getAllMatchedCardSchemes(cardNumber->clearSpaces)
  allMatchedCards
  ->Array.find(card => isCardSchemeEnabled(~cardScheme=card, ~enabledCardSchemes))
  ->Option.getOr("")
}

// Get eligible co-badged card schemes
let getEligibleCoBadgedCardSchemes = (~matchedCardSchemes, ~enabledCardSchemes) => {
  matchedCardSchemes->Array.filter(ele => enabledCardSchemes->Array.includes(ele))
}

// Determine card brand from card number
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

// Get maximum card length for a card brand
let maxCardLength = cardBrand => {
  let obj = getobjFromCardPattern(cardBrand)
  Array.reduce(obj.length, 0, (acc, val) => acc > val ? acc : val)
}

// Validate card number using Luhn algorithm and length check
let cardValid = (cardNumber, cardBrand) => {
  let clearValue = cardNumber->clearSpaces
  Array.includes(getobjFromCardPattern(cardBrand).length, clearValue->String.length) &&
  calculateLuhn(cardNumber)
}

// Check if card number equals maximum length
let isCardNumberEqualsMax = (cardNumber, cardBrand) => {
  let clearValue = cardNumber->clearSpaces
  clearValue->String.length == maxCardLength(cardBrand) || clearValue->String.length == 16
}

// Format card number with spaces based on card type
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

// Format CVC number based on card type
let formatCVCNumber = (val, cardType) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardType)
  clearValue->slice(0, obj.maxCVCLength)
}

// Check if CVC length is in valid range for card brand
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

// Check if CVC equals maximum length for card brand
let cvcNumberEqualsMaxLength = (val, cardBrand) => {
  let clearValue = val->clearSpaces
  let obj = getobjFromCardPattern(cardBrand)
  let cvcMaxLengthEquals = clearValue->String.length == obj.maxCVCLength
  cvcMaxLengthEquals
}

// Complete CVC validation
let checkCardCVC = (cvcNumber, cardBrand) => {
  cvcNumber->String.length > 0 && cvcNumberInRange(cvcNumber, cardBrand)
}

// Check if CVC is at maximum length
let checkMaxCardCvv = (cvcNumber, cardBrand) => {
  cvcNumber->String.length > 0 && cvcNumberEqualsMaxLength(cvcNumber, cardBrand)
}
