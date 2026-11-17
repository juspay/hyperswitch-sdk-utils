open SharedLoggerTypes

type entry = {
  row: row,
  backfill: bool,
  mutable sessionIdAtEnqueue: string,
  mutable awaitingInitialSession: bool,
  mutable pmSessionId: string,
}

let rows: array<entry> = []
let flushTimer = ref(None)
let droppedRows = ref(0)
let failures = ref(0)
let sendFailures = ref(0)
let send = ref((_payload: string) => false)
let initialSessionWaitStarted = ref(false)
let initialSessionDeadline = ref(None)

let awaitInitialSession = () =>
  if !initialSessionWaitStarted.contents {
    initialSessionWaitStarted := true
    initialSessionDeadline := Some(Date.now() +. Float.fromInt(LoggerConfig.defaultTimeoutMs))
  }

let waitingForInitialSession = () =>
  switch initialSessionDeadline.contents {
  | Some(deadline) if Date.now() < deadline => true
  | Some(_) => {
      initialSessionDeadline := None
      false
    }
  | None => false
  }

let bindInitialSession = sessionId => {
  if sessionId !== "" && waitingForInitialSession() {
    initialSessionDeadline := None
    rows->Array.forEach(entry => {
      if entry.awaitingInitialSession {
        entry.sessionIdAtEnqueue = sessionId
        entry.awaitingInitialSession = false
      }
    })
  }
}

let configure = (~sendPayload) => send := sendPayload

let clearTimer = () =>
  flushTimer.contents->Option.forEach(timer => {
    clearTimeout(timer)
    flushTimer := None
  })

let utf8ByteLength = value => {
  let bytes = ref(0)
  let index = ref(0)
  while index.contents < value->String.length {
    let codePoint = value->String.codePointAt(index.contents)->Option.getOr(0)
    let width = codePoint <= 0x7F ? 1 : codePoint <= 0x7FF ? 2 : codePoint <= 0xFFFF ? 3 : 4
    bytes := bytes.contents + width
    index := index.contents + (codePoint > 0xFFFF ? 2 : 1)
  }
  bytes.contents
}

let sendNext = () => {
  let batch = []
  let serializedRows = []
  let bytes = ref(2)
  let consumed = ref(0)
  let full = ref(false)
  let serializationFailed = ref(false)
  while (
    !full.contents &&
    consumed.contents < rows->Array.length &&
    batch->Array.length < LoggerConfig.batchSize
  ) {
    let entry = rows->Array.getUnsafe(consumed.contents)
    // The log API currently only accepts payment_id, not PM-session ID.
    let paymentId = entry.row.paymentId !== "" ? entry.row.paymentId : entry.pmSessionId
    let serialized = try {
      {...entry.row, paymentId}->JSON.stringifyAny->Option.getOr("")
    } catch {
    | _ => ""
    }
    let rowBytes = serialized->utf8ByteLength
    let nextBytes = bytes.contents + rowBytes + (batch->Array.length > 0 ? 1 : 0)
    if serialized === "" {
      batch->Array.push(entry)
      consumed := consumed.contents + 1
      serializationFailed := true
      full := true
    } else if rowBytes + 2 > LoggerConfig.maxBatchBytes {
      consumed := consumed.contents + 1
      droppedRows := droppedRows.contents + 1
    } else if nextBytes > LoggerConfig.maxBatchBytes {
      full := true
    } else {
      batch->Array.push(entry)
      serializedRows->Array.push(serialized)
      bytes := nextBytes
      consumed := consumed.contents + 1
    }
  }
  rows->Array.splice(~start=0, ~remove=consumed.contents, ~insert=[])
  let sent = try {
    !serializationFailed.contents &&
    (batch->Array.length === 0 || send.contents(`[${serializedRows->Array.join(",")}]`))
  } catch {
  | _ => false
  }
  sent ? None : Some(batch)
}

let trimOverflow = () => {
  while rows->Array.length > LoggerConfig.maxQueuedRows {
    let disposable = rows->Array.findIndex(entry => entry.row.logType !== "ERROR")
    rows->Array.splice(~start=disposable === -1 ? 0 : disposable, ~remove=1, ~insert=[])
    droppedRows := droppedRows.contents + 1
  }
}

let requeue = batch => {
  rows->Array.splice(~start=0, ~remove=0, ~insert=batch)
  trimOverflow()
}

let rec flush = () => {
  clearTimer()
  if rows->Array.length > 0 && waitingForInitialSession() {
    scheduleFlush()
  } else if rows->Array.length > 0 {
    switch sendNext() {
    | None => failures := 0
    | Some(batch) => {
        failures := failures.contents + 1
        sendFailures := sendFailures.contents + 1
        if failures.contents >= LoggerConfig.maxSendAttemptsPerBatch {
          failures := 0
          droppedRows := droppedRows.contents + batch->Array.length
        } else {
          batch->requeue
        }
      }
    }
    if rows->Array.length > 0 {
      scheduleFlush()
    }
  }
}
and scheduleFlush = () =>
  if flushTimer.contents->Option.isNone {
    let delay = switch failures.contents {
    | 0 => LoggerConfig.flushDelayMs
    | failures => Math.Int.min(
        LoggerConfig.flushDelayMs * Math.Int.pow(2, ~exp=failures),
        LoggerConfig.maxFlushBackoffMs,
      )
    }
    flushTimer := Some(setTimeout(flush, delay))
  }

let drain = () => {
  clearTimer()
  let failed = ref(false)
  while !failed.contents && rows->Array.length > 0 {
    switch sendNext() {
    | None => ()
    | Some(batch) => {
        batch->requeue
        failed := true
      }
    }
  }
  if rows->Array.length > 0 {
    scheduleFlush()
  }
}

let push = (row, ~backfill=true, ~pmSessionId="", ~sessionIdAtEnqueue="") => {
  rows->Array.push({
    row,
    backfill,
    pmSessionId,
    sessionIdAtEnqueue,
    awaitingInitialSession: backfill && sessionIdAtEnqueue === "" && waitingForInitialSession(),
  })
  if rows->Array.length > LoggerConfig.maxQueuedRows {
    trimOverflow()
  }
  scheduleFlush()
}

let backfillContext = (context: context) =>
  rows->Array.forEach(entry => {
    let row = entry.row
    let fill = (current, value) =>
      entry.backfill &&
      entry.sessionIdAtEnqueue === context.sessionId &&
      current === "" && value !== ""
        ? value
        : current
    row.sessionId = fill(row.sessionId, context.sessionId)
    row.merchantId = fill(row.merchantId, context.merchantId)
    row.paymentId = fill(row.paymentId, context.paymentId)
    entry.pmSessionId = fill(entry.pmSessionId, context.pmSessionId)
    row.authenticationId = fill(row.authenticationId, context.authenticationId)
  })
