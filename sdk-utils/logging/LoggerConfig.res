let severityRank = (severity: SharedLoggerTypes.severity) =>
  switch severity {
  | Debug => 0
  | Info => 1
  | Warning => 2
  | Error => 3
  }

let enabled = ref(false)
let endpoint = ref("")
let minimumRank = ref(severityRank(Debug))
let maxRowsPerEventName = ref(100)
let maxUserEventsPerName = ref(20)

let configure = (
  ~enableLogging=?,
  ~logEndpoint=?,
  ~loggingLevel=?,
  ~maxLogsPushedPerEventName=?,
  ~maxUserEventsPerName as userEventLimit: option<int>=?,
  (),
) => {
  enableLogging->Option.forEach(value => enabled := value)
  logEndpoint->Option.forEach(value => endpoint := value->String.trim)
  loggingLevel->Option.forEach(value =>
    minimumRank :=
      switch value->String.trim->String.toUpperCase {
      | "INFO" => severityRank(Info)
      | "WARNING" | "WARN" => severityRank(Warning)
      | "ERROR" => severityRank(Error)
      | "SILENT" => severityRank(Error) + 1
      | _ => severityRank(Debug)
      }
  )
  maxLogsPushedPerEventName->Option.forEach(value => maxRowsPerEventName := Math.Int.max(1, value))
  userEventLimit->Option.forEach(value => maxUserEventsPerName := Math.Int.max(1, value))
}

let schemaVersion = 9
let defaultTimeoutMs = 30000
let userGatedTimeoutMs = 600000

let flushDelayMs = 3000
let maxFlushBackoffMs = 60000
let batchSize = 10
let maxBatchBytes = 64 * 1024
let maxQueuedRows = 100
let maxSendAttemptsPerBatch = 4

let maxTextLength = 256
let maxRowTextLength = 1024
let maxDetailBytes = 8192
let maxPayloadFields = 120
