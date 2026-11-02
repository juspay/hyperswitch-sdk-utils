open SharedLoggerTypes

type entry = {
  row: row,
  backfill: bool,
  mutable pmSessionId: string,
}

let rows: array<entry> = []
let flushTimer = ref(None)
let droppedRows = ref(0)
let failures = ref(0)
let sendFailures = ref(0)
let send = ref((_payload: string) => false)

let configure = (~sendPayload) => send := sendPayload

let clearTimer = () =>
  flushTimer.contents->Option.forEach(timer => {
    clearTimeout(timer)
    flushTimer := None
  })

let sendNext = () => {
  let count = Math.Int.min(rows->Array.length, LoggerConfig.batchSize)
  let batch = rows->Array.slice(~start=0, ~end=count)
  rows->Array.splice(~start=0, ~remove=count, ~insert=[])
  // The log API currently only accepts payment_id, not PM-session ID.
  let sent = try {
    count === 0 || send.contents(
      `[${batch->Array.map(entry => {
        let paymentId = entry.row.paymentId !== "" ? entry.row.paymentId : entry.pmSessionId
        {...entry.row, paymentId}->JSON.stringifyAny->Option.getOr("")
      })->Array.join(",")}]`,
    )
  } catch {
  | _ => false
  }
  sent ? None : Some(batch)
}

let requeue = batch => {
  rows->Array.splice(~start=0, ~remove=0, ~insert=batch)
  let overflow = rows->Array.length - LoggerConfig.maxQueuedRows
  if overflow > 0 {
    rows->Array.splice(~start=LoggerConfig.maxQueuedRows, ~remove=overflow, ~insert=[])
    droppedRows := droppedRows.contents + overflow
  }
}

let rec flush = () => {
  clearTimer()
  if rows->Array.length > 0 {
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

let push = (row, ~backfill=true, ~pmSessionId="") => {
  rows->Array.push({row, backfill, pmSessionId})
  if rows->Array.length > LoggerConfig.maxQueuedRows {
    rows->Array.splice(~start=0, ~remove=rows->Array.length - LoggerConfig.maxQueuedRows, ~insert=[])
    droppedRows := droppedRows.contents + 1
  }
  scheduleFlush()
}

let backfillContext = (context: context) =>
  rows->Array.forEach(entry => {
    let row = entry.row
    let fill = (current, value) =>
      entry.backfill && current === "" && value !== "" ? value : current
    row.sessionId = fill(row.sessionId, context.sessionId)
    row.merchantId = fill(row.merchantId, context.merchantId)
    row.paymentId = fill(row.paymentId, context.paymentId)
    entry.pmSessionId = fill(entry.pmSessionId, context.pmSessionId)
    row.authenticationId = fill(row.authenticationId, context.authenticationId)
  })
