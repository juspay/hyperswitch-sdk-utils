open ValidationUtils
@val external sdkUrl: string = "sdkUrl"
let isInteg = sdkUrl === "https://dev.hyperswitch.io"
let isSandbox = sdkUrl === "https://beta.hyperswitch.io" || sdkUrl === "http://localhost:9050"
let checkIsTestCardWildcard = val => ["1111222233334444"]->Array.includes(val)

let cardValid = (cardNumber, cardBrand) => {
  if cardBrand == "" && (isInteg || isSandbox) {
    checkIsTestCardWildcard(cardNumber)
  } else {
    CardNumberValidation.cardValid(cardNumber, cardBrand)
  }
}
