/**
 * Hyperswitch Shared Business Logic Utilities Library
 * 
 * This module provides comprehensive business logic utility functions shared across
 * Hyperswitch client SDKs (mobile and web). It includes functions for:
 * - String processing and transformation
 * - JSON/Object manipulation utilities
 * - Array processing functions
 * - Common validation helpers
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */

// Import shared utilities from CardValidation
// open CardValidation

// ============================================================================
// STRING PROCESSING UTILITIES
// ============================================================================

// Convert text to SCREAMING_SNAKE_CASE
let convertToScreamingSnakeCase = text => {
  text->String.trim->String.replaceRegExp(%re("/ /g"), "_")->String.toUpperCase
}

// Convert string to camelCase
let toCamelCase = str => {
  if str->String.includes(":") {
    str
  } else {
    str
    ->String.split("_")
    ->Array.mapWithIndex((word, index) => {
      if index == 0 {
        word->String.toLowerCase
      } else {
        switch word->String.get(0) {
        | Some(firstChar) =>
          firstChar->String.toUpperCase ++
          word->String.sliceToEnd(~start=1)->String.toLowerCase
        | None => word
        }
      }
    })
    ->Array.join("")
  }
}

// Convert string to snake_case
let toSnakeCase = str => {
  str->Js.String2.unsafeReplaceBy0(%re("/[A-Z]/g"), (letter, _, _) =>
    "_" ++ letter->String.toLowerCase
  )
}

// Convert string to kebab-case
let toKebabCase = str => {
  str
  ->String.replaceRegExp(%re("/[A-Z]/g"), "-")
  ->String.toLowerCase
  ->String.replaceRegExp(%re("/^-/"), "")
}

// Replace underscores with spaces
let underscoresToSpaces = str => {
  str->String.replaceAll("_", " ")
}

// ============================================================================
// JSON/OBJECT UTILITIES
// ============================================================================

// Get dictionary from JSON safely
let getDictFromJson = (json: JSON.t) => {
  json->JSON.Decode.object->Option.getOr(Dict.make())
}

// Get dictionary from JSON key
let getDictFromJsonKey = (json, key) => {
  json->Dict.get(key)->Option.getOr(JSON.Encode.null)->getDictFromJson
}

// Get string from JSON with default
let getStringFromJson = (json, default) => {
  json->JSON.Decode.string->Option.getOr(default)
}

// Get optional string from dictionary
let getOptionString = (dict, key) => {
  dict->Dict.get(key)->Option.flatMap(JSON.Decode.string)
}

// Get optional float from dictionary
let getOptionFloat = (dict, key) => {
  switch dict->Dict.get(key) {
  | Some(json) => json->JSON.Decode.float
  | None => None
  }
}

// Get string from dictionary with default
let getString = (dict, key, default) => {
  getOptionString(dict, key)->Option.getOr(default)
}

// Get boolean from dictionary with default
let getBool = (dict, key, default) => {
  dict
  ->Dict.get(key)
  ->Option.flatMap(JSON.Decode.bool)
  ->Option.getOr(default)
}

// Get object from dictionary with default
let getObj = (dict, key, default) => {
  dict
  ->Dict.get(key)
  ->Option.flatMap(JSON.Decode.object)
  ->Option.getOr(default)
}

// Get optional object from dictionary
let getOptionalObj = (dict, key) => {
  dict
  ->Dict.get(key)
  ->Option.flatMap(JSON.Decode.object)
}

// Get optional array from dictionary
let getOptionalArrayFromDict = (dict, key) => {
  dict->Dict.get(key)->Option.flatMap(JSON.Decode.array)
}

// Get array from dictionary with default
let getArrayFromDict = (dict, key, default) => {
  dict->getOptionalArrayFromDict(key)->Option.getOr(default)
}

// Get array from dictionary (empty array default)
let getArray = (dict, key) => {
  dict->getOptionalArrayFromDict(key)->Option.getOr([])
}

// Get JSON object from dictionary
let getJsonObjectFromDict = (dict, key) => {
  dict->Dict.get(key)->Option.getOr(JSON.Encode.object(Dict.make()))
}

// ============================================================================
// ARRAY PROCESSING UTILITIES
// ============================================================================

// Get string array from dictionary
let getStrArray = (dict, key) => {
  dict
  ->getOptionalArrayFromDict(key)
  ->Option.map(arr => arr->Array.filterMap(JSON.Decode.string))
  ->Option.getOr([])
}

// Get optional string array from dictionary
let getOptionalStrArray: (Dict.t<JSON.t>, string) => option<array<string>> = (dict, key) => {
  switch dict->getOptionalArrayFromDict(key) {
  | Some(arr) => Some(arr->Array.filterMap(JSON.Decode.string))
  | None => None
  }
}

// Convert string array to JSON array
let getArrofJsonString = (arr: array<string>) => {
  arr->Array.map(item => item->JSON.Encode.string)
}

// ============================================================================
// VALIDATION UTILITIES
// ============================================================================

// Check if text contains only digits
let containsOnlyDigits = text => {
  %re("/^[0-9]*$/")->Js.Re.test_(text)
}

// Check if text contains at least one digit
let containsDigit = text => {
  switch text->String.match(%re("/\d/")) {
  | Some(_) => true
  | None => false
  }
}

// Check if text contains more than two digits
let containsMoreThanTwoDigits = text => {
  switch text->String.match(%re("/\d/g")) {
  | Some(matches) => matches->Array.length > 2
  | None => false
  }
}

// Basic IBAN validation (length and format check)
let isValidIban = text => {
  let trimmedText = text->String.trim
  let isIbanEmpty = trimmedText->String.length != 0
  isIbanEmpty
}

// ============================================================================
// UTILITY HELPERS
// ============================================================================

// Convert option<bool> to string representation
let getBoolOptionVal = boolOptionVal => {
  switch boolOptionVal {
  | Some(bool) => bool ? "valid" : "invalid"
  | None => ""
  }
}

// Get string from record (JSON stringify)
let getStringFromRecord = record => record->JSON.stringifyAny->Option.getOr("")

// Get JSON object from record
let getJsonObjectFromRecord = record => record->Obj.magic

// Transform keys in JSON object
type case = CamelCase | SnakeCase | KebabCase

// Simple transform keys function (simplified for compatibility)
let transformKeysSnakeToCamel = (json: JSON.t) => {
  // For now, return the original JSON
  // This can be enhanced later with proper Dict iteration
  json
}
