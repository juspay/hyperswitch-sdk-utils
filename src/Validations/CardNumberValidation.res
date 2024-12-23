open ValidationUtils

let maxCardLength = cardBrand => {
  let obj = getobjFromCardPattern(cardBrand)
  Array.reduce(obj.length, 0, (acc, val) => acc > val ? acc : val)
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
  mod(totalSum, 10) == 0 || ["3000100811111072", "4000100511112003"]->Array.includes(card) // test cards
}

let cardValid = (cardNumber, cardBrand) => {
  let clearValue = cardNumber->clearSpaces
  Array.includes(getobjFromCardPattern(cardBrand).length, clearValue->String.length) &&
  calculateLuhn(cardNumber)
}
