type defaults = {
  nativeDdcStartedSeverity: SharedLoggerTypes.severity,
  webDdcStartedSeverity: SharedLoggerTypes.severity,
  nativeDecodeConstructorPayload: bool,
  webDecodeConstructorPayload: bool,
  schemaVersion: int,
  maxRowsPerEventName: int,
  maxUserEventsPerName: int,
  defaultTimeoutMs: int,
  userGatedTimeoutMs: int,
  flushDelayMs: int,
  maxFlushBackoffMs: int,
  maxBatchBytes: int,
  maxQueuedRows: int,
  maxSendAttemptsPerBatch: int,
  sendTimeoutMs: int,
  maxTextLength: int,
  maxRowTextLength: int,
  maxDetailBytes: int,
  maxPayloadFields: int,
  maxConfigBytes: int,
  maxConfigDepth: int,
  maxConfigArrayItems: int,
  maxConfigOmitted: int,
  minimumRank: int,
  loggingLevel: string,
  enableLogging: bool,
  endpoint: string,
  idleTimeoutMs: int,
  logsPath: string,
  nonProdLogsPath: string,
  nativeUnmaskedFields: array<string>,
  webUnmaskedFields: array<string>,
  webMaskOverrides: array<string>,
}

@module("./LoggerDefaults.json") external defaults: defaults = "default"

module Common = {
  let schemaVersion = defaults.schemaVersion
  let defaultTimeoutMs = defaults.defaultTimeoutMs
  let userGatedTimeoutMs = defaults.userGatedTimeoutMs
  let flushDelayMs = defaults.flushDelayMs
  let maxFlushBackoffMs = defaults.maxFlushBackoffMs
  let maxBatchBytes = defaults.maxBatchBytes
  let maxQueuedRows = defaults.maxQueuedRows
  let maxSendAttemptsPerBatch = defaults.maxSendAttemptsPerBatch
  let sendTimeoutMs = defaults.sendTimeoutMs
  let maxTextLength = defaults.maxTextLength
  let maxRowTextLength = defaults.maxRowTextLength
  let maxDetailBytes = defaults.maxDetailBytes
  let maxPayloadFields = defaults.maxPayloadFields
  let maxConfigBytes = defaults.maxConfigBytes
  let maxConfigDepth = defaults.maxConfigDepth
  let maxConfigArrayItems = defaults.maxConfigArrayItems
  let maxConfigOmitted = defaults.maxConfigOmitted
  let idleTimeoutMs = defaults.idleTimeoutMs
}

// No clamping or new fallback is applied to injected numeric values.
let rankOfLevel = value =>
  switch value->String.trim->String.toUpperCase {
  | "INFO" => 1
  | "WARNING" | "WARN" => 2
  | "ERROR" => 3
  | "SILENT" => 4
  | _ => defaults.minimumRank
  }

let normalizeEndpoint = value => value->String.trim

let selectNativeEndpoint = (~customBackend, ~customLogs, ~defaultUrl) =>
  switch (customBackend, customLogs) {
  | (Some(_), None) => None
  | (_, Some(url)) => Some(url)
  | (None, None) => Some(defaultUrl())
  }

type transport = {endpoint: ref<string>, headers: ref<Dict.t<string>>}
let makeTransport = () => {endpoint: ref(defaults.endpoint), headers: ref(Dict.make())}

let prepareNativeHeaders = (headers, ~platform) => {
  headers->Dict.set("Content-Type", "application/json")
  headers->Dict.set("X-Client-Platform", platform)
  headers
}

let isPathStartsWithPattern = (path, pattern) =>
  path === pattern || path->String.startsWith(pattern ++ ".")

let normalizePath = path => path->String.replaceRegExp(%re("/\[(\d+)\]/g"), "")

let shouldMask = (~fields, ~overrides, ~normalize, path) => {
  let path = normalize(path)
  let isOverridden = overrides->Array.includes(path)
  let isExcluded = fields->Array.some(pattern => isPathStartsWithPattern(path, normalize(pattern)))
  isOverridden || !isExcluded
}

module Native = {
  let ddcStartedSeverity = defaults.nativeDdcStartedSeverity
  let decodeConstructorPayload = defaults.nativeDecodeConstructorPayload
  include Common
  let minimumRank = defaults.minimumRank
  let maxRowsPerEventName = defaults.maxRowsPerEventName
  let maxUserEventsPerName = defaults.maxUserEventsPerName
  let fieldsToExcludeFromMasking = defaults.nativeUnmaskedFields
  let shouldMaskField = path =>
    shouldMask(~fields=fieldsToExcludeFromMasking, ~overrides=[], ~normalize=value => value, path)
}

module Web = {
  let ddcStartedSeverity = defaults.webDdcStartedSeverity
  let decodeConstructorPayload = defaults.webDecodeConstructorPayload
  include Common
  let fieldsToExcludeFromMasking = defaults.webUnmaskedFields
  let overrideFieldsToExcludeFromMasking = defaults.webMaskOverrides
  let shouldMaskField = path =>
    shouldMask(
      ~fields=fieldsToExcludeFromMasking,
      ~overrides=overrideFieldsToExcludeFromMasking,
      ~normalize=normalizePath,
      path,
    )
}
