/**
 * Hyperswitch Shared API Response Processor
 * 
 * Handles processing and parsing of API responses across all platforms.
 * This module provides consistent response handling and error processing.
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */
open BusinessLogicUtils
open ApiTypes

// ============================================================================
// PAYMENT STATUS PROCESSING
// ============================================================================

/**
 * Parses payment status string into typed enum
 * @param status - Status string from API response
 * @returns Typed payment status
 */
let parsePaymentStatus = (status: string): paymentStatus => {
  switch status {
  | "succeeded" => Succeeded
  | "failed" => Failed
  | "processing" => Processing
  | "requires_customer_action" => RequiresCustomerAction
  | "requires_capture" => RequiresCapture
  | "requires_confirmation" => RequiresConfirmation
  | "requires_merchant_action" => RequiresMerchantAction
  | "cancelled" => Cancelled
  | other => Unknown(other)
  }
}

/**
 * Converts payment status back to string
 * @param status - Typed payment status
 * @returns Status string
 */
let paymentStatusToString = (status: paymentStatus): string => {
  switch status {
  | Succeeded => "succeeded"
  | Failed => "failed"
  | Processing => "processing"
  | RequiresCustomerAction => "requires_customer_action"
  | RequiresCapture => "requires_capture"
  | RequiresConfirmation => "requires_confirmation"
  | RequiresMerchantAction => "requires_merchant_action"
  | Cancelled => "cancelled"
  | Unknown(str) => str
  }
}

// ============================================================================
// NEXT ACTION PROCESSING
// ============================================================================

/**
 * Parses next action from API response
 * @param response - API response JSON
 * @returns Parsed next action object
 */
let parseNextAction = (response: JSON.t): nextAction => {
  let dict = response->getDictFromJson
  let nextActionDict =
    dict->Dict.get("next_action")->Option.flatMap(JSON.Decode.object)->Option.getOr(Dict.make())

  {
    type_: nextActionDict->getString("type", ""),
    redirectToUrl: nextActionDict->getString("redirect_to_url", ""),
    data: nextActionDict->Dict.get("data"),
  }
}

/**
 * Extracts redirect URL from next action
 * @param nextAction - Next action object
 * @returns Redirect URL if available
 */
let getRedirectUrl = (nextAction: option<nextAction>): option<string> => {
  switch nextAction {
  | Some(action) if action.redirectToUrl != "" => Some(action.redirectToUrl)
  | _ => None
  }
}

/**
 * Checks if next action requires redirect
 * @param nextAction - Next action object
 * @returns True if redirect is required
 */
let requiresRedirect = (nextAction: option<nextAction>): bool => {
  switch nextAction {
  | Some(action) =>
    action.type_ == "redirect_to_url" ||
    action.type_ == "redirect_inside_popup" ||
    action.redirectToUrl != ""
  | None => false
  }
}

// ============================================================================
// ERROR PROCESSING
// ============================================================================

/**
 * Parses error from API response
 * @param response - API response JSON
 * @returns Parsed error object if present
 */
let parseApiError = (response: JSON.t): option<apiError> => {
  let dict = response->getDictFromJson

  switch dict->Dict.get("error") {
  | Some(errorJson) => {
      let errorDict = errorJson->JSON.Decode.object->Option.getOr(Dict.make())
      Some({
        code: errorDict->getString("code", ""),
        message: errorDict->getString("message", "Unknown error"),
        type_: errorDict->getString("type", ""),
        status: dict->getString("status", "failed"),
      })
    }
  | None => None
  }
}

/**
 * Creates default error for failed responses
 * @param statusCode - HTTP status code
 * @param message - Error message
 * @returns Default error object
 */
let createDefaultError = (~statusCode: int, ~message: string): apiError => {
  {
    code: statusCode->Int.toString,
    message,
    type_: "api_error",
    status: "failed",
  }
}

// ============================================================================
// RESPONSE PROCESSING
// ============================================================================

/**
 * Processes payment API response into structured format
 * @param response - Raw API response
 * @returns Processed payment response
 */
let processPaymentResponse = (response: apiResponse<JSON.t>): processedPaymentResponse => {
  let dict = response.data->getDictFromJson
  let status = dict->getString("status", "")->parsePaymentStatus

  let nextAction = switch dict->Dict.get("next_action") {
  | Some(_) => Some(parseNextAction(response.data))
  | None => None
  }

  let error = if response.status >= 400 {
    // HTTP error status
    switch parseApiError(response.data) {
    | Some(err) => Some(err)
    | None => Some(createDefaultError(~statusCode=response.status, ~message="API request failed"))
    }
  } else {
    // Check for business logic errors in successful HTTP response
    parseApiError(response.data)
  }

  {
    status,
    nextAction,
    error,
    rawResponse: response.data,
  }
}

/**
 * Checks if response indicates success
 * @param processed - Processed payment response
 * @returns True if payment was successful
 */
let isSuccessResponse = (processed: processedPaymentResponse): bool => {
  switch (processed.status, processed.error) {
  | (Succeeded, None) => true
  | _ => false
  }
}

/**
 * Checks if response indicates failure
 * @param processed - Processed payment response
 * @returns True if payment failed
 */
let isFailureResponse = (processed: processedPaymentResponse): bool => {
  switch (processed.status, processed.error) {
  | (Failed, _) => true
  | (_, Some(_)) => true
  | _ => false
  }
}

/**
 * Checks if response requires customer action
 * @param processed - Processed payment response
 * @returns True if customer action is required
 */
let requiresCustomerAction = (processed: processedPaymentResponse): bool => {
  switch processed.status {
  | RequiresCustomerAction => true
  | _ => false
  }
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Extracts payment ID from client secret
 * @param clientSecret - Client secret string
 * @returns Payment ID
 */
let getPaymentIdFromClientSecret = (clientSecret: string): string => {
  clientSecret
  ->String.split("_secret_")
  ->Array.get(0)
  ->Option.getOr("")
}

/**
 * Builds standard headers for API requests
 * @param publishableKey - Publishable key
 * @param appId - Optional app ID
 * @returns Headers dictionary
 */
let buildStandardHeaders = (~publishableKey: string, ~appId: option<string>=?): Dict.t<string> => {
  let headers = Dict.make()
  headers->Dict.set("Content-Type", "application/json")
  headers->Dict.set("api-key", publishableKey)

  switch appId {
  | Some(id) => headers->Dict.set("x-app-id", id->String.replace(".hyperswitch://", ""))
  | None => ()
  }

  headers
}

/**
 * Merges custom headers with standard headers
 * @param standardHeaders - Standard headers
 * @param customHeaders - Custom headers to merge
 * @returns Merged headers dictionary
 */
let mergeHeaders = (standardHeaders: Dict.t<string>, customHeaders: Dict.t<string>): Dict.t<
  string,
> => {
  let merged = Dict.make()

  // Copy standard headers
  standardHeaders
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    merged->Dict.set(key, value)
  })

  // Override with custom headers
  customHeaders
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    merged->Dict.set(key, value)
  })

  merged
}
