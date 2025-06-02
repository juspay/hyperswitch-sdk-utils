/**
 * Hyperswitch Shared Date/Time Utilities Library
 * 
 * This module provides comprehensive date and time utility functions shared across
 * Hyperswitch client SDKs (mobile and web). It includes functions for:
 * - Date parsing and formatting
 * - Expiry date processing and validation
 * - Current date/time utilities
 * - Date comparison and validation logic
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */

// Import shared utilities from CardValidation
open CardValidation

// Get string from array at specific index with fallback
let getStrFromIndex = (arr: array<string>, index) => {
  arr->Array.get(index)->Option.getOr("")
}

// Parse current month and year from ISO date string
let getCurrentMonthAndYear = (dateTimeIsoString: string) => {
  let tempTimeDateString = dateTimeIsoString->String.replace("Z", "")
  let tempTimeDate = tempTimeDateString->String.split("T")

  let date = tempTimeDate->Array.get(0)->Option.getOr("")
  let dateComponents = date->String.split("-")

  let currentMonth = dateComponents->Array.get(1)->Option.getOr("")
  let currentYear = dateComponents->Array.get(0)->Option.getOr("")

  (currentMonth->toInt, currentYear->toInt)
}

// Split expiry date string into month and year components
let splitExpiryDates = val => {
  let split = val->String.split("/")
  let value = split->Array.map(item => item->String.trim)
  let month = value->Array.get(0)->Option.getOr("")
  let year = value->Array.get(1)->Option.getOr("")
  (month, year)
}

// Get formatted expiry dates with proper year prefix
let getExpiryDates = val => {
  let date = Date.make()->Date.toISOString
  let (month, year) = splitExpiryDates(val)
  let (_, currentYear) = getCurrentMonthAndYear(date)
  let prefix = currentYear->Int.toString->String.slice(~start=0, ~end=2)
  (month, `${prefix}${year}`)
}

// Check if expiry date input is complete (MM/YY format)
let isExpiryComplete = val => {
  let (month, year) = splitExpiryDates(val)
  month->String.length == 2 && year->String.length == 2
}

// Format card expiry number with automatic MM/YY formatting
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

// Validate expiry date against current date
let getExpiryValidity = cardExpiry => {
  let date = Date.make()->Date.toISOString
  let (month, year) = getExpiryDates(cardExpiry)
  let (currentMonth, currentYear) = getCurrentMonthAndYear(date)
  let valid = if currentYear == year->toInt && month->toInt >= currentMonth && month->toInt <= 12 {
    true
  } else if (
    year->toInt > currentYear && 
    year->toInt < currentYear + 100 && 
    month->toInt >= 1 && 
    month->toInt <= 12
  ) {
    true
  } else {
    false
  }
  valid
}

// Complete expiry validation (length + validity + completeness)
let isExpiryValid = val => {
  val->String.length > 0 && getExpiryValidity(val) && isExpiryComplete(val)
}

// Check if expiry date is valid (basic validation)
let checkCardExpiry = expiry => {
  expiry->String.length > 0 && getExpiryValidity(expiry)
}

// Format expiry to two-digit year
let formatExpiryToTwoDigit = expiry => {
  if expiry->String.length == 2 {
    expiry
  } else {
    expiry->String.slice(~start=2, ~end=4)
  }
}

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
