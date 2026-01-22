let getOptionString = (dict, key) => {
  dict->Dict.get(key)->Option.flatMap(JSON.Decode.string)
}

let getString = (dict, key, default) => {
  getOptionString(dict, key)->Option.getOr(default)
}

let getStringFromJson = (json, default) => {
  json->JSON.Decode.string->Option.getOr(default)
}

let getInt = (dict, key, default: int) => {
  dict
  ->Dict.get(key)
  ->Option.flatMap(JSON.Decode.float)
  ->Option.getOr(default->Int.toFloat)
  ->Float.toInt
}

let getFloatFromString = (str, default) => str->Float.fromString->Option.getOr(default)

let getFloatFromJson = (json, default) => {
  switch json->JSON.Classify.classify {
  | String(str) => getFloatFromString(str, default)
  | Number(floatValue) => floatValue
  | _ => default
  }
}

let getFloat = (dict, key, default) => {
  dict->Dict.get(key)->Option.map(json => getFloatFromJson(json, default))->Option.getOr(default)
}

let getJsonBoolValue = (dict, key, default) => {
  dict->Dict.get(key)->Option.getOr(default->JSON.Encode.bool)
}

let getJsonStringFromDict = (dict, key, default) => {
  dict->Dict.get(key)->Option.getOr(default->JSON.Encode.string)
}

let getJsonArrayFromDict = (dict, key, default) => {
  dict->Dict.get(key)->Option.getOr(default->JSON.Encode.array)
}
let getJsonFromDict = (dict, key, default) => {
  dict->Dict.get(key)->Option.getOr(default)
}

let getJsonObjFromDict = (dict, key, default) => {
  dict->Dict.get(key)->Option.flatMap(JSON.Decode.object)->Option.getOr(default)
}
let getDecodedStringFromJson = (json, callbackFunc, defaultValue) => {
  json
  ->JSON.Decode.object
  ->Option.flatMap(callbackFunc)
  ->Option.flatMap(JSON.Decode.string)
  ->Option.getOr(defaultValue)
}

let getDecodedBoolFromJson = (json, callbackFunc, defaultValue) => {
  json
  ->JSON.Decode.object
  ->Option.flatMap(callbackFunc)
  ->Option.flatMap(JSON.Decode.bool)
  ->Option.getOr(defaultValue)
}

let getDictFromObj = (dict, key) => {
  dict->Dict.get(key)->Option.flatMap(JSON.Decode.object)->Option.getOr(Dict.make())
}

let getJsonObjectFromDict = (dict, key) => {
  dict->Dict.get(key)->Option.getOr(JSON.Encode.object(Dict.make()))
}
let getOptionBool = (dict, key) => {
  dict->Dict.get(key)->Option.flatMap(JSON.Decode.bool)
}
let getDictFromJson = (json: JSON.t) => {
  json->JSON.Decode.object->Option.getOr(Dict.make())
}

let getDictFromDict = (dict, key) => {
  dict->getJsonObjectFromDict(key)->getDictFromJson
}

let getBool = (dict, key, default) => {
  getOptionBool(dict, key)->Option.getOr(default)
}

let getOptionsDict = options => options->Option.getOr(JSON.Encode.null)->getDictFromJson

let getOptionalArrayFromDict = (dict, key) => {
  dict->Dict.get(key)->Option.flatMap(JSON.Decode.array)
}
let getArray = (dict, key) => {
  dict->getOptionalArrayFromDict(key)->Option.getOr([])
}

let getStrArray = (dict, key) => {
  dict
  ->getOptionalArrayFromDict(key)
  ->Option.getOr([])
  ->Array.map(json => json->getStringFromJson(""))
}

let convertDictToArrayOfKeyStringTuples = dict => {
  dict
  ->Dict.toArray
  ->Array.map(entries => {
    let (x, val) = entries
    (x, val->JSON.Decode.string->Option.getOr(""))
  })
}

let getStringFromOptionalJson = (json, default) => {
  json->Option.flatMap(JSON.Decode.string)->Option.getOr(default)
}

let snakeToPascalCase = str => {
  let words = str->String.split("_")
  words
  ->Array.filter(item => item->String.length > 0)
  ->Array.map(item => {
    let firstChar = item->String.charAt(0)->String.toUpperCase
    let restOfString = item->String.length > 1 ? item->String.sliceToEnd(~start=1) : ""
    firstChar ++ restOfString
  })
  ->Array.join("")
}

let getArrayElement = (arr: array<'a>, index: int, default: 'a): 'a => {
  arr->Array.get(index)->Option.getOr(default)
}

let rec mergeDict = (dict1: Dict.t<JSON.t>, dict2: Dict.t<JSON.t>): Dict.t<JSON.t> => {
  let mergedDict = dict1->Dict.copy
  dict2
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    switch (mergedDict->Dict.get(key), value) {
    | (Some(existingValue), newValue) =>
      switch (existingValue->JSON.Decode.object, newValue->JSON.Decode.object) {
      | (Some(existingObj), Some(newObj)) =>
        let mergedObj = mergeDict(existingObj, newObj)
        mergedDict->Dict.set(key, mergedObj->JSON.Encode.object)
      | _ => mergedDict->Dict.set(key, newValue)
      }
    | (None, newValue) => mergedDict->Dict.set(key, newValue)
    }
  })
  mergedDict
}

let getDisplayName = text => {
  let transformText = switch text {
  | "credit" => "Card"
  | "crypto_currency" => "Crypto"
  | "afterpay_clearpay" => "Afterpay"
  | "bnb_smart_chain" => "BNB Smart Chain"
  | "ach" | "sepa" | "bacs" | "becs" => text ++ " Debit"
  | other => other
  }

  let capitalizeWord = str =>
    switch str->String.get(0) {
    | Some(firstChar) => firstChar->String.toUpperCase ++ str->String.sliceToEnd(~start=1)
    | None => str
    }

  transformText
  ->String.split("_")
  ->Array.map(capitalizeWord)
  ->Array.join(" ")
}
