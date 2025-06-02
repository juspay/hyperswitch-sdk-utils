/**
 * Hyperswitch Shared Payment API Core
 * 
 * Core API functions for payment operations shared across all platforms.
 * This module provides the main API calls for payment processing.
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */

open BusinessLogicUtils
open ApiTypes
open ApiResponseProcessor
// open ApiLogger

// ============================================================================
// CORE PAYMENT API FUNCTIONS
// ============================================================================

/**
 * Confirms a payment with the Hyperswitch API
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param body - Request body as JSON string
 * @param endpoint - API endpoint base URL
 * @param appId - Optional app identifier
 * @param customHeaders - Optional custom headers
 * @returns Promise with API response
 */
let confirmPayment = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~body: string,
  ~endpoint: string,
  ~appId: option<string>=?,
  ~customHeaders: Dict.t<string>=Dict.make(),
) => {
  let paymentId = getPaymentIdFromClientSecret(clientSecret)
  let uri = `${endpoint}/payments/${paymentId}/confirm`
  
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  let finalHeaders = mergeHeaders(headers, customHeaders)
  
  fetchApi(~uri, ~method="POST", ~headers=finalHeaders, ~bodyStr=body)
}

/**
 * Retrieves payment status from the Hyperswitch API
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param isForceSync - Whether to force synchronous retrieval
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let retrievePayment = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~isForceSync: bool=false,
  ~appId: option<string>=?,
) => {
  let paymentId = getPaymentIdFromClientSecret(clientSecret)
  let forceSync = isForceSync ? "true" : "false"
  let uri = `${endpoint}/payments/${paymentId}?force_sync=${forceSync}&client_secret=${clientSecret}`
  
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  
  fetchApi(~uri, ~method="GET", ~headers, ~bodyStr="")
}

/**
 * Fetches available payment methods
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let fetchPaymentMethods = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~appId: option<string>=?,
) => {
  let uri = `${endpoint}/account/payment_methods?client_secret=${clientSecret}`
  
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  
  fetchApi(~uri, ~method="GET", ~headers, ~bodyStr="")
}

/**
 * Fetches customer's saved payment methods
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let fetchCustomerPaymentMethods = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~appId: option<string>=?,
) => {
  let uri = `${endpoint}/customers/payment_methods?client_secret=${clientSecret}`
  
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  
  fetchApi(~uri, ~method="GET", ~headers, ~bodyStr="")
}

/**
 * Creates session tokens for wallet payments
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param wallets - Array of wallet configurations
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let createSessionTokens = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~wallets: array<JSON.t>=[],
  ~appId: option<string>=?,
) => {
  let paymentId = getPaymentIdFromClientSecret(clientSecret)
  let uri = `${endpoint}/payments/session_tokens`
  
  let body = [
    ("payment_id", paymentId->JSON.Encode.string),
    ("client_secret", clientSecret->JSON.Encode.string),
    ("wallets", wallets->JSON.Encode.array),
  ]->Dict.fromArray->JSON.Encode.object->JSON.stringify
  
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  
  fetchApi(~uri, ~method="POST", ~headers, ~bodyStr=body)
}

/**
 * Performs 3DS authentication
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param threeDsData - 3DS authentication data
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let authenticate3DS = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~threeDsData: JSON.t,
  ~appId: option<string>=?,
) => {
  let paymentId = getPaymentIdFromClientSecret(clientSecret)
  let uri = `${endpoint}/payments/${paymentId}/3ds/authentication`
  
  let body = [
    ("client_secret", clientSecret->JSON.Encode.string),
    ("device_channel", "BRW"->JSON.Encode.string),
  ]->Dict.fromArray->JSON.Encode.object
  
  // Merge with 3DS data
  let bodyDict = body->getDictFromJson
  let threeDsDict = threeDsData->getDictFromJson
  threeDsDict->Dict.toArray->Array.forEach(((key, value)) => {
    bodyDict->Dict.set(key, value)
  })
  
  let finalBody = bodyDict->JSON.Encode.object->JSON.stringify
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  
  fetchApi(~uri, ~method="POST", ~headers, ~bodyStr=finalBody)
}

// ============================================================================
// PAYMENT METHOD MANAGEMENT
// ============================================================================

/**
 * Deletes a saved payment method
 * @param fetchApi - Platform-specific fetch function
 * @param paymentMethodId - ID of payment method to delete
 * @param ephemeralKey - Ephemeral key for authentication
 * @param endpoint - API endpoint base URL
 * @returns Promise with API response
 */
let deletePaymentMethod = (
  ~fetchApi: fetchFunction,
  ~paymentMethodId: string,
  ~ephemeralKey: string,
  ~endpoint: string,
) => {
  let uri = `${endpoint}/payment_methods/${paymentMethodId}`
  
  let headers = Dict.make()
  headers->Dict.set("Content-Type", "application/json")
  headers->Dict.set("api-key", ephemeralKey)
  
  fetchApi(~uri, ~method="DELETE", ~headers, ~bodyStr="")
}

/**
 * Saves a payment method for future use
 * @param fetchApi - Platform-specific fetch function
 * @param paymentMethodId - ID of payment method to save
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param saveData - Data for saving payment method
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let savePaymentMethod = (
  ~fetchApi: fetchFunction,
  ~paymentMethodId: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~saveData: JSON.t,
  ~appId: option<string>=?,
) => {
  let uri = `${endpoint}/payment_methods/${paymentMethodId}/save`
  
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  let body = saveData->JSON.stringify
  
  fetchApi(~uri, ~method="POST", ~headers, ~bodyStr=body)
}

// ============================================================================
// ADVANCED PAYMENT OPERATIONS
// ============================================================================

/**
 * Completes payment authorization (for specific flows)
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param authData - Authorization completion data
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let completeAuthorization = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~authData: JSON.t,
  ~appId: option<string>=?,
) => {
  let paymentId = getPaymentIdFromClientSecret(clientSecret)
  let uri = `${endpoint}/payments/${paymentId}/complete_authorize`
  
  let body = [
    ("client_secret", clientSecret->JSON.Encode.string),
  ]->Dict.fromArray->JSON.Encode.object
  
  // Merge with auth data
  let bodyDict = body->getDictFromJson
  let authDict = authData->getDictFromJson
  authDict->Dict.toArray->Array.forEach(((key, value)) => {
    bodyDict->Dict.set(key, value)
  })
  
  let finalBody = bodyDict->JSON.Encode.object->JSON.stringify
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  
  fetchApi(~uri, ~method="POST", ~headers, ~bodyStr=finalBody)
}

/**
 * Posts session tokens (for specific payment flows)
 * @param fetchApi - Platform-specific fetch function
 * @param clientSecret - Payment client secret
 * @param publishableKey - Merchant publishable key
 * @param endpoint - API endpoint base URL
 * @param sessionData - Session token data
 * @param appId - Optional app identifier
 * @returns Promise with API response
 */
let postSessionTokens = (
  ~fetchApi: fetchFunction,
  ~clientSecret: string,
  ~publishableKey: string,
  ~endpoint: string,
  ~sessionData: JSON.t,
  ~appId: option<string>=?,
) => {
  let paymentId = getPaymentIdFromClientSecret(clientSecret)
  let uri = `${endpoint}/payments/${paymentId}/post_session_tokens`
  
  let body = [
    ("client_secret", clientSecret->JSON.Encode.string),
    ("payment_id", paymentId->JSON.Encode.string),
  ]->Dict.fromArray->JSON.Encode.object
  
  // Merge with session data
  let bodyDict = body->getDictFromJson
  let sessionDict = sessionData->getDictFromJson
  sessionDict->Dict.toArray->Array.forEach(((key, value)) => {
    bodyDict->Dict.set(key, value)
  })
  
  let finalBody = bodyDict->JSON.Encode.object->JSON.stringify
  let headers = buildStandardHeaders(~publishableKey, ~appId?)
  
  fetchApi(~uri, ~method="POST", ~headers, ~bodyStr=finalBody)
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Builds request body for payment confirmation
 * @param clientSecret - Payment client secret
 * @param paymentData - Payment method data
 * @param returnUrl - Return URL for redirects
 * @param browserInfo - Browser information
 * @returns JSON string for request body
 */
let buildConfirmPaymentBody = (
  ~clientSecret: string,
  ~paymentData: JSON.t,
  ~returnUrl: string,
  ~browserInfo: option<JSON.t>=?,
) => {
  let body = [
    ("client_secret", clientSecret->JSON.Encode.string),
    ("return_url", returnUrl->JSON.Encode.string),
  ]->Dict.fromArray
  
  // Merge payment data
  let paymentDict = paymentData->getDictFromJson
  paymentDict->Dict.toArray->Array.forEach(((key, value)) => {
    body->Dict.set(key, value)
  })
  
  // Add browser info if provided
  switch browserInfo {
  | Some(info) => {
      let infoDict = info->getDictFromJson
      infoDict->Dict.toArray->Array.forEach(((key, value)) => {
        body->Dict.set(key, value)
      })
    }
  | None => ()
  }
  
  body->JSON.Encode.object->JSON.stringify
}
