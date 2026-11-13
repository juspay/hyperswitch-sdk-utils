let enabled = ref(false)
let endpoint = ref("")
let minimumRank = ref(0)
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
      | "INFO" => 1
      | "WARNING" | "WARN" => 2
      | "ERROR" => 3
      | "SILENT" => 4
      | _ => 0
      }
  )
  maxLogsPushedPerEventName->Option.forEach(value => maxRowsPerEventName := Math.Int.max(1, value))
  userEventLimit->Option.forEach(value => maxUserEventsPerName := Math.Int.max(1, value))
}

let schemaVersion = 8
let defaultTimeoutMs = 30000
let userGatedTimeoutMs = 600000

let flushDelayMs = 3000
let maxFlushBackoffMs = 60000
let batchSize = 10
let maxQueuedRows = 100
let maxSendAttemptsPerBatch = 4

let maxTextLength = 256
let maxRowTextLength = 1024
let maxDetailBytes = 8192
let maxPayloadFields = 120
