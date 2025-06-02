/**
 * Hyperswitch Shared API Logger
 * 
 * Provides consistent logging functionality for API operations across all platforms.
 * This module standardizes log formatting and metadata collection for API calls.
 * 
 * @version 1.0.0
 * @author Hyperswitch Team
 */

open ApiTypes

// ============================================================================
// LOGGING UTILITIES
// ============================================================================

/**
 * Creates structured log data for API calls
 * @param url - The API endpoint URL
 * @param method - HTTP method (GET, POST, etc.)
 * @param statusCode - HTTP status code
 * @param apiLogType - Type of log (Request, Response, NoResponse, Err)
 * @param data - Response or error data
 * @param eventName - Event identifier for tracking
 * @param logType - Log level (INFO, ERROR, etc.)
 * @returns Structured log data object
 */
let logApiCall = (
  ~url: string,
  ~method: string,
  ~statusCode: string,
  ~apiLogType: apiLogType,
  ~data: JSON.t,
  ~eventName: string,
  ~logType: string,
): logData => {
  let (value, internalMetadata) = switch apiLogType {
  | Request => ([("url", url->JSON.Encode.string)], [])
  | Response => (
      [("url", url->JSON.Encode.string), ("statusCode", statusCode->JSON.Encode.string)],
      [("response", data)],
    )
  | NoResponse => (
      [
        ("url", url->JSON.Encode.string), 
        ("statusCode", "504"->JSON.Encode.string),
        ("response", data)
      ],
      [("response", data)],
    )
  | Err => (
      [
        ("url", url->JSON.Encode.string), 
        ("statusCode", statusCode->JSON.Encode.string),
        ("response", data)
      ],
      [("response", data)],
    )
  }
  
  {
    logType: logType,
    eventName: eventName,
    value: apiLogType->JSON.stringifyAny->Option.getOr(""),
    internalMetadata: internalMetadata->Dict.fromArray->JSON.Encode.object->JSON.stringify,
  }
}

/**
 * Creates log data for API request initiation
 * @param url - The API endpoint URL
 * @param eventName - Event identifier
 * @returns Log data for request start
 */
let logApiRequest = (~url: string, ~eventName: string): logData => {
  logApiCall(
    ~url,
    ~method="",
    ~statusCode="",
    ~apiLogType=Request,
    ~data=JSON.Encode.null,
    ~eventName,
    ~logType="INFO",
  )
}

/**
 * Creates log data for successful API response
 * @param url - The API endpoint URL
 * @param statusCode - HTTP status code
 * @param data - Response data
 * @param eventName - Event identifier
 * @returns Log data for successful response
 */
let logApiResponse = (~url: string, ~statusCode: string, ~data: JSON.t, ~eventName: string): logData => {
  logApiCall(
    ~url,
    ~method="",
    ~statusCode,
    ~apiLogType=Response,
    ~data,
    ~eventName,
    ~logType="INFO",
  )
}

/**
 * Creates log data for API error
 * @param url - The API endpoint URL
 * @param statusCode - HTTP status code
 * @param error - Error data
 * @param eventName - Event identifier
 * @returns Log data for error response
 */
let logApiError = (~url: string, ~statusCode: string, ~error: JSON.t, ~eventName: string): logData => {
  logApiCall(
    ~url,
    ~method="",
    ~statusCode,
    ~apiLogType=Err,
    ~data=error,
    ~eventName,
    ~logType="ERROR",
  )
}

/**
 * Creates log data for network timeout/no response
 * @param url - The API endpoint URL
 * @param error - Error data
 * @param eventName - Event identifier
 * @returns Log data for no response
 */
let logApiNoResponse = (~url: string, ~error: JSON.t, ~eventName: string): logData => {
  logApiCall(
    ~url,
    ~method="",
    ~statusCode="504",
    ~apiLogType=NoResponse,
    ~data=error,
    ~eventName,
    ~logType="ERROR",
  )
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Creates error JSON from string message
 * @param message - Error message
 * @returns JSON formatted error
 */
let createErrorJson = (message: string): JSON.t => {
  [("error", message->JSON.Encode.string)]
  ->Dict.fromArray
  ->JSON.Encode.object
}
