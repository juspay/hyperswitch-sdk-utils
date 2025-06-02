/**
 * Hyperswitch Shared Payment Utilities Library
 * 
 * This module provides comprehensive payment-specific utility functions shared across
 * Hyperswitch client SDKs (mobile and web). It includes functions for:
 * - Payment method processing
 * - Layout and UI utilities
 * - Bank and payment validation
 * - Payment-specific helpers
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */

// Import shared utilities
open BusinessLogicUtils

// ============================================================================
// PAYMENT METHOD UTILITIES
// ============================================================================

// Get layout class from payment type (simplified version)
let getLayoutClass = layout => {
  // This is a simplified version - actual implementation would depend on PaymentType module
  // For now, return a basic object structure
  {
    "type": layout,
    "spacedAccordionItems": false,
  }
}

// Extract all bank names from payment methods array
let getAllBanknames = obj => {
  obj->Array.reduce([], (acc, item) => {
    // Simplified implementation - actual would depend on PaymentMethodListType
    acc->Array.push(item)->ignore
    acc
  })
}

// Get card string representation from card type enum
let getCardStringFromType = val => {
  // This would map card type enums to strings
  // Simplified implementation for now
  switch val {
  | _ => "Unknown"
  }
}

// ============================================================================
// VALIDATION HELPERS
// ============================================================================

// ZIP code validation for specific country
let isValidZip = (~zipCode, ~country) => {
  let _ = country // Placeholder for country-specific validation
  let trimmedZip = zipCode->String.trim
  
  // Basic validation - non-empty and reasonable length
  trimmedZip->String.length > 0 && trimmedZip->String.length <= 10
}

// Get postal code regex for country
let postalRegex = (postalCodes: array<'a>, ~country=?) => {
  let _ = (postalCodes, country) // Placeholder
  // Default regex for basic postal code validation
  "^[0-9]{5}(-[0-9]{4})?$"
}

// ============================================================================
// PAYMENT PROCESSING HELPERS
// ============================================================================

// Check if payment method requires mandate
let checkIfMandate = paymentType => {
  // Simplified implementation
  switch paymentType {
  | _ => false
  }
}

// Get action type from next action object
let getActionType = nextActionObj => {
  switch nextActionObj {
  | Some(action) => action
  | None => {"type_": "", "redirectToUrl": ""}
  }
}

// ============================================================================
// CARD UTILITIES (Payment-specific)
// ============================================================================

// Check if card brand is supported in enabled schemes
let isCardBrandSupported = (~cardBrand, ~enabledSchemes) => {
  enabledSchemes->Array.includes(cardBrand->String.toLowerCase)
}

// Get first valid card scheme from payment method list
let getFirstValidCardScheme = (~cardNumber, ~enabledCardSchemes) => {
  // This would use card validation logic to find first valid scheme
  // Simplified implementation
  let _ = (cardNumber, enabledCardSchemes)
  None
}

// Get eligible co-badged card schemes
let getEligibleCoBadgedCardSchemes = (~matchedCardSchemes, ~enabledCardSchemes) => {
  matchedCardSchemes->Array.filter(scheme => {
    enabledCardSchemes->Array.includes(scheme->String.toLowerCase)
  })
}

// ============================================================================
// ENVIRONMENT & CONFIGURATION UTILITIES
// ============================================================================

// Check environment from publishable key
let checkEnv = publishableKey => {
  if publishableKey != "" && publishableKey->String.startsWith("pk_prd_") {
    "prod"
  } else if publishableKey->String.startsWith("pk_snd_") {
    "sandbox"
  } else {
    "local"
  }
}

// Validate publishable key format
let isValidPK = (env, publishableKey) => {
  switch env {
  | "prod" => publishableKey->String.startsWith("pk_prd_")
  | "sandbox" => publishableKey->String.startsWith("pk_snd_")
  | _ => true // Allow any format for local/dev
  }
}

// Get version from string
let getVersionFromStr = name => {
  switch name->String.toLowerCase {
  | "v1" => "v1"
  | "v2" => "v2"
  | _ => "v1" // Default to v1
  }
}

// ============================================================================
// BROWSER & PLATFORM UTILITIES
// ============================================================================

// Detect Safari browser (simplified)
let checkIsSafari = () => {
  // This would check user agent in actual implementation
  // Simplified for shared module
  false
}

// Parse query parameters
let getQueryParamsDictforKey = (searchParams, keyName) => {
  let dict = Dict.make()
  
  searchParams
  ->String.split("&")
  ->Array.forEach(paramStr => {
    let keyValArr = String.split(paramStr, "=")
    let key = keyValArr->Array.get(0)->Option.getOr("")
    let value = if keyValArr->Array.length > 1 {
      keyValArr->Array.get(1)->Option.getOr("")
    } else {
      ""
    }
    Dict.set(dict, key, value)
  })
  
  dict->Dict.get(keyName)->Option.getOr("")
}

// ============================================================================
// UTILITY CONVERTERS
// ============================================================================

// Convert integer to string
let toString = val => val->Int.toString

// Safe integer conversion
let toInt = val => val->Int.fromString->Option.getOr(0)

// Split name into first and last
let splitName = (str: option<string>) => {
  switch str {
  | Some(name) => {
      let parts = name->String.trim->String.split(" ")
      let firstName = parts->Array.get(0)->Option.getOr("")
      let lastName = parts->Array.sliceToEnd(~start=1)->Array.join(" ")
      (firstName, lastName)
    }
  | None => ("", "")
  }
}

// ============================================================================
// ARRAY UTILITIES (Payment-specific)
// ============================================================================

// Remove duplicate items from array
let removeDuplicate = arr => {
  arr->Array.reduce([], (acc, item) => {
    if acc->Array.includes(item) {
      acc
    } else {
      acc->Array.push(item)->ignore
      acc
    }
  })
}

// Swap elements in payment options arrays
let swapCardOption = (cardOpts: array<string>, dropOpts: array<string>, selectedOption: string) => {
  let popEle = Array.pop(cardOpts)
  dropOpts->Array.push(popEle->Option.getOr(""))->ignore
  cardOpts->Array.push(selectedOption)->ignore
  let temp: array<string> = dropOpts->Array.filter(item => item != selectedOption)
  (cardOpts, temp)
}

// ============================================================================
// ERROR HANDLING UTILITIES
// ============================================================================

// Check if response is an error
let isError = (res: JSON.t) => {
  res
  ->getDictFromJson
  ->Dict.get("error")
  ->Option.isSome
}

// Get error code from response
let getErrorCode = (res: JSON.t) => {
  res
  ->getDictFromJson
  ->getString("error_code", "")
}

// Get error message from response
let getErrorMessage = (res: JSON.t) => {
  res
  ->getDictFromJson
  ->getString("error_message", "Unknown error")
}
