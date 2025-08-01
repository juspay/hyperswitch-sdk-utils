open CountryStateDataHookTypes

let formatPhoneNumber = (phoneNumberText, countries) => {
  if phoneNumberText == "" || phoneNumberText->String.length > 20 {
    ("", phoneNumberText)
  } else {
    let startsAt =
      phoneNumberText->String.search(%re("/[+\uFF0B0-9\uFF10-\uFF19\u0660-\u0669\u06F0-\u06F9]/"))

    if startsAt < 0 {
      ("", phoneNumberText)
    } else {
      let cleanNumber =
        phoneNumberText
        ->String.sliceToEnd(~start=startsAt)
        ->String.replaceRegExp(%re("/[^0-9\uFF10-\uFF19\u0660-\u0669\u06F0-\u06F9#]+$/"), "")

      if (
        cleanNumber->String.length < 2 ||
          !(cleanNumber->String.match(%re("/[0-9]/"))->Option.isSome)
      ) {
        ("", phoneNumberText)
      } else {
        let hasPlus = cleanNumber->String.indexOf("+") !== -1
        let digits = cleanNumber->String.replaceRegExp(%re("/[^0-9]/g"), "")

        let (countryCode, nationalNumber) = if hasPlus {
          let rec findCode = (i, digits, countries) => {
            if i > 3 || i > digits->String.length {
              ("", digits)
            } else {
              let code = digits->String.slice(~start=0, ~end=i)
              let found = countries->Array.find(country => {
                country.phone_number_code === `+${code}`
              })

              found->Option.isSome
                ? (code, digits->String.sliceToEnd(~start=i))
                : findCode(i + 1, digits, countries)
            }
          }
          findCode(1, digits, countries)
        } else {
          ("", digits)
        }

        if hasPlus && countryCode == "" {
          // ||
          // nationalNumber->String.length < 2 ||
          // nationalNumber->String.length > 17
          ("", phoneNumberText)
        } else if countryCode == "" {
          ("", nationalNumber)
        } else {
          (`+${countryCode}`, nationalNumber)
        }
      }
    }
  }
}
