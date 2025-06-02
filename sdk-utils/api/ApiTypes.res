/**
 * Hyperswitch Shared API Types
 * 
 * Common type definitions for API operations across all Hyperswitch client SDKs.
 * This module provides type safety and consistency for API requests and responses.
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */

// ============================================================================
// API LOGGING TYPES
// ============================================================================

type apiLogType = Request | Response | NoResponse | Err

// ============================================================================
// API RESPONSE TYPES
// ============================================================================

type apiResponse<'a> = {
  status: int,
  data: 'a,
  headers: Dict.t<string>,
}

type apiError = {
  code: string,
  message: string,
  type_: string,
  status: string,
}

// ============================================================================
// PAYMENT TYPES
// ============================================================================

type confirmParams = {
  return_url: string,
  publishableKey: string,
  redirect: option<string>,
}

type paymentStatus = 
  | Succeeded
  | Failed  
  | Processing
  | RequiresCustomerAction
  | RequiresCapture
  | RequiresConfirmation
  | RequiresMerchantAction
  | Cancelled
  | Unknown(string)

type nextAction = {
  type_: string,
  redirectToUrl: string,
  data: option<JSON.t>,
}

type processedPaymentResponse = {
  status: paymentStatus,
  nextAction: option<nextAction>,
  error: option<apiError>,
  rawResponse: JSON.t,
}

// ============================================================================
// API CONFIGURATION TYPES
// ============================================================================

type apiConfig = {
  endpoint: string,
  headers: Dict.t<string>,
  timeout: option<int>,
}

// ============================================================================
// FETCH FUNCTION TYPE
// ============================================================================

// Generic fetch function that both platforms can implement
type fetchFunction = (
  ~uri: string,
  ~method: string,
  ~headers: Dict.t<string>,
  ~bodyStr: string,
) => promise<apiResponse<JSON.t>>

// ============================================================================
// LOG DATA TYPE
// ============================================================================

type logData = {
  logType: string,
  eventName: string,
  value: string,
  internalMetadata: string,
}
