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
