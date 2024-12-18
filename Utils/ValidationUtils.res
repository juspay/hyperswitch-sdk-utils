let clearSpaces = value => {
  value->String.replaceRegExp(%re("/\D+/g"), "")
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

let toInt = val => val->Int.fromString->Option.getOr(0)
