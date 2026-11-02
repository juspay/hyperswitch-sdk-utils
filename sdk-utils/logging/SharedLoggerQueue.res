module type Config = {
  let maxBatchBytes: int
  let maxQueuedRows: int
  let flushDelayMs: int
  let maxFlushBackoffMs: int
}
module Make = (LoggerConfig: Config) => {
  let clearTimer = flushTimer =>
    flushTimer.contents->Option.forEach(timer => {
      clearTimeout(timer)
      flushTimer := None
    })
  let batchSize = (rows, ~bytesOf) => {
    let bytes = ref(2)
    let count = ref(0)
    while (
      count.contents < rows->Array.length &&
        (count.contents === 0 ||
          bytes.contents + bytesOf(rows->Array.getUnsafe(count.contents)) <=
            LoggerConfig.maxBatchBytes)
    ) {
      bytes := bytes.contents + bytesOf(rows->Array.getUnsafe(count.contents))
      count := count.contents + 1
    }
    count.contents
  }
  let requeue = (rows, batch, droppedRows) => {
    rows->Array.splice(~start=0, ~remove=0, ~insert=batch)
    let overflow = rows->Array.length - LoggerConfig.maxQueuedRows
    if overflow > 0 {
      rows->Array.splice(~start=LoggerConfig.maxQueuedRows, ~remove=overflow, ~insert=[])
      droppedRows := droppedRows.contents + overflow
    }
  }
  let flushDelay = failures =>
    switch failures {
    | 0 => LoggerConfig.flushDelayMs
    | failures =>
      Math.Int.min(
        LoggerConfig.flushDelayMs * Math.Int.pow(2, ~exp=failures),
        LoggerConfig.maxFlushBackoffMs,
      )
    }
}

module type EngineConfig = {
  include Config
  let maxSendAttemptsPerBatch: int
}
module type Host = {
  type contextType
  let native: bool
  let identifiers: contextType => SharedLoggerPlatform.identity
  let utf8Length: string => int
  let sendAsync: string => promise<bool>
  let sendSync: string => bool
  let installLifecycle: (unit => unit) => unit
}
module Engine = (LoggerConfig: EngineConfig, Host: Host) => {
  module Policy = Make(LoggerConfig)
  module Utils = SharedLoggerUtils.Make(SharedLoggerConfig.Native)
  type entry = {
    row: SharedLoggerTypes.row,
    backfill: bool,
    mutable pmSessionId: string,
    mutable text: string,
    mutable bytes: int,
  }
  let rows: array<entry> = []
  let queuedBytes = ref(0)
  let flushTimer = ref(None)
  let errorPending = ref(false)
  let sending = ref(false)
  let droppedRows = ref(0)
  let failures = ref(0)
  let sendFailures = ref(0)
  let clearTimer = () => Policy.clearTimer(flushTimer)
  let serialize = entry => {
    let text = entry.row->SharedLoggerSerialization.serializeRow
    if Host.native {
      entry.text = text
    }
    entry.bytes = text->Host.utf8Length + 1
  }
  let recountBytes = () =>
    queuedBytes := rows->Array.reduce(0, (total, entry) => total + entry.bytes)
  let batchSize = () => Policy.batchSize(rows, ~bytesOf=entry => entry.bytes)
  let takeBatch = () => {
    let count = batchSize()
    let batch = rows->Array.slice(~start=0, ~end=count)
    rows->Array.splice(~start=0, ~remove=count, ~insert=[])
    recountBytes()
    batch
  }

  let payloadOf = batch => `[${batch->Array.map(entry => entry.text)->Array.join(",")}]`

  let sendNext = () => {
    let count = batchSize()
    let batch = rows->Array.slice(~start=0, ~end=count)
    rows->Array.splice(~start=0, ~remove=count, ~insert=[])
    // Keep both identifiers distinct in the queue; the log API currently only accepts payment_id.
    count === 0 ||
      Host.sendSync(
        `[${batch
          ->Array.map(entry => {
            let paymentId = entry.row.paymentId !== "" ? entry.row.paymentId : entry.pmSessionId
            {...entry.row, paymentId}->SharedLoggerSerialization.serializeRow
          })
          ->Array.join(",")}]`,
      )
      ? None
      : Some(batch)
  }

  let requeue = batch => Policy.requeue(rows, batch, droppedRows)
  let rec flush = () => {
    if Host.native {
      clearTimer()
      if !sending.contents {
        errorPending := false
        switch takeBatch() {
        | [] => queuedBytes := 0
        | batch =>
          sending := true
          batch
          ->payloadOf
          ->Host.sendAsync
          ->Promise.thenResolve(delivered => {
            sending := false
            if delivered {
              failures := 0
            } else {
              failures := failures.contents + 1
              sendFailures := sendFailures.contents + 1
              if failures.contents >= LoggerConfig.maxSendAttemptsPerBatch {
                failures := 0
                droppedRows := droppedRows.contents + batch->Array.length
              } else {
                batch->requeue
              }
            }
            recountBytes()
            if rows->Array.length > 0 {
              errorPending.contents ? flush() : scheduleFlush()
            }
          })
          ->ignore
        }
      }
    } else {
      clearTimer()
      errorPending := false
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
        recountBytes()
        if rows->Array.length > 0 {
          scheduleFlush()
        }
      } else {
        queuedBytes := 0
      }
    }
  }
  and scheduleFlush = () =>
    if flushTimer.contents->Option.isNone {
      let delay = Policy.flushDelay(failures.contents)
      flushTimer := Some(setTimeout(flush, delay))
    }

  let drain = () =>
    Utils.safeRun(() => {
      if Host.native {
        clearTimer()
        errorPending := false
        while rows->Array.length > 0 {
          let batch = takeBatch()
          batch
          ->payloadOf
          ->Host.sendAsync
          ->Promise.thenResolve(delivered =>
            if !delivered {
              batch->requeue
              recountBytes()
              scheduleFlush()
            }
          )
          ->ignore
        }
      } else {
        clearTimer()
        errorPending := false
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
        recountBytes()
        if rows->Array.length > 0 {
          scheduleFlush()
        }
      }
    })
  let push = (row, ~isError, ~backfill=true, ~pmSessionId="") => {
    let entry = {row, backfill, pmSessionId, text: "", bytes: 0}
    entry->serialize
    rows->Array.push(entry)
    queuedBytes := queuedBytes.contents + entry.bytes
    if isError {
      if !errorPending.contents {
        errorPending := true
        setTimeout(() => errorPending.contents ? flush() : (), 0)->ignore
      }
    } else if queuedBytes.contents >= LoggerConfig.maxBatchBytes {
      flush()
    } else {
      scheduleFlush()
    }
  }

  let backfillContext = (context: Host.contextType) => {
    let context = Host.identifiers(context)
    rows->Array.forEach(entry => {
      let row = entry.row
      let changed = ref(false)
      let fill = (current, value) =>
        if (Host.native || entry.backfill) && current === "" && value !== "" {
          changed := true
          value
        } else {
          current
        }
      row.sessionId = fill(row.sessionId, context.sessionId)
      row.merchantId = fill(row.merchantId, context.merchantId)
      row.paymentId = fill(row.paymentId, context.paymentId)
      if !Host.native {
        entry.pmSessionId = fill(entry.pmSessionId, context.pmSessionId)
      }
      row.authenticationId = fill(row.authenticationId, context.authenticationId)
      if changed.contents {
        let before = entry.bytes
        entry->serialize
        queuedBytes := queuedBytes.contents - before + entry.bytes
      }
    })
  }
  Host.installLifecycle(drain)
}
