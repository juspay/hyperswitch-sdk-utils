open SharedLoggerTypes

type platform = {
  component: string,
  version: string,
  appId: string,
  platform: string,
  userAgent: string,
  browserName: string,
  browserVersion: string,
  href: string,
  getContext: unit => context,
  fillContext: (context, context) => context,
  safeRun: (unit => unit) => unit,
  summarizeExn: exn => errorSummary,
  summarizeErrorResponse: JSON.t => option<errorSummary>,
}

type t = {
  platform: platform,
  counts: ref<Dict.t<int>>,
  onceKeys: ref<Dict.t<bool>>,
}

let make = platform => {platform, counts: ref(Dict.make()), onceKeys: ref(Dict.make())}

let reset = logger => {
  logger.counts := Dict.make()
  logger.onceKeys := Dict.make()
}

let rank = severity =>
  switch severity {
  | Debug => 0
  | Info => 1
  | Warning => 2
  | Error => 3
  }

let isEnabled = severity =>
  LoggerConfig.enabled.contents && severity->rank >= LoggerConfig.minimumRank.contents

let emit = (
  logger,
  ~category,
  ~spec: eventSpec,
  ~severity: severity,
  ~data: option<details>=?,
  ~details: option<details>=?,
  ~verbatim: option<details>=?,
  ~exn: option<exn>=?,
  ~failure: option<JSON.t>=?,
  ~durationMs: option<float>=?,
  ~paymentMethod: option<SharedLoggerPaymentMethod.paymentMethod>=?,
  ~context: option<context>=?,
  ~source: string,
  ~limit: option<int>=?,
  ~message: option<string>=?,
  ~once=false,
) =>
  if severity->isEnabled {
    let platform = logger.platform
    platform.safeRun(() => {
      let data = data->Option.getOr([])
      let details = details->Option.getOr([])
      let context = switch context {
      | Some(started) if started.scoped => started
      | Some(started) => started->platform.fillContext(platform.getContext())
      | None => platform.getContext()
      }
      let eventName = SharedLoggerEvent.eventName(
        ~category,
        ~action=spec.action,
        ~subject=spec.subject,
        ~outcome=spec.outcome,
      )
      let limit = switch (limit, category) {
      | (None, User) => Some(LoggerConfig.maxUserEventsPerName.contents)
      | _ => limit
      }
      let cap = limit->Option.getOr(LoggerConfig.maxRowsPerEventName.contents)
      let eventSeen = logger.counts.contents->Dict.get(eventName)->Option.getOr(0)

      let capReached = limit->Option.isSome && eventSeen === cap
      let (name, severity, data, details, message) = capReached
        ? (
            SharedLoggerEvent.eventName(
              ~category,
              ~action=Fact,
              ~subject="event_limit_reached",
              ~outcome=None,
            ),
            Warning,
            [("limited_event", eventName->JSON.Encode.string), ("limit", cap->JSON.Encode.int)],
            [],
            None,
          )
        : (eventName, severity, data, details, message)
      let seen = capReached ? 0 : eventSeen
      let onceKey = once
        ? name ++
          "#" ++
          data
          ->Array.concat(details)
          ->Array.concat(verbatim->Option.getOr([]))
          ->Dict.fromArray
          ->JSON.stringifyAny
          ->Option.getOr("")
        : ""
      let duplicateOfOnce = once && logger.onceKeys.contents->Dict.get(onceKey)->Option.isSome
      if once {
        logger.onceKeys.contents->Dict.set(onceKey, true)
      }

      if eventSeen <= cap && !duplicateOfOnce {
        logger.counts.contents->Dict.set(eventName, eventSeen + 1)
        let errorDetails = switch exn {
        | Some(exn) => Some(exn->platform.summarizeExn)
        | None => failure->Option.flatMap(platform.summarizeErrorResponse)
        }->Option.mapOr([], SharedLoggerFormatting.errorDetails)
        let details =
          SharedLoggerFormatting.mergeDetails(~data, ~details)
          ->Array.concat(errorDetails)
          ->SharedLoggerFormatting.normalizeDetails(~maxTextLength=LoggerConfig.maxTextLength)
          ->Array.concat(verbatim->Option.getOr([]))
        let counter = (key, counter) => {
          let count = counter.contents
          counter := 0
          if count > 0 {
            details->Array.push((key, count->JSON.Encode.int))
          }
        }
        counter("dropped_rows", LoggerQueue.droppedRows)
        counter("send_failures", LoggerQueue.sendFailures)
        if limit->Option.isNone && seen === cap {
          details->Array.push(("rate_limited", true->JSON.Encode.bool))
        }

        let value =
          {
            schemaVersion: LoggerConfig.schemaVersion,
            href: platform.href,
            occurrence: seen + 1,
            message: ?(
              message->Option.map(message =>
                message
                ->SharedLoggerFormatting.redact
                ->SharedLoggerFormatting.truncateTo(LoggerConfig.maxTextLength)
              )
            ),
            details: details
            ->SharedLoggerFormatting.fitToBudget(~maxDetailBytes=LoggerConfig.maxDetailBytes)
            ->Dict.fromArray,
          }
          ->rowValueToJson
          ->JSON.stringify

        LoggerQueue.push(
          {
            timestamp: Date.now()->Float.toString,
            logType: (severity :> string),
            component: platform.component,
            category: (category :> string),
            source,
            version: platform.version,
            value,
            sessionId: context.sessionId,
            merchantId: context.merchantId,
            paymentId: context.paymentId,
            authenticationId: context.authenticationId,
            appId: platform.appId,
            platform: platform.platform,
            userAgent: platform.userAgent,
            eventName: name,
            browserName: platform.browserName,
            browserVersion: platform.browserVersion,
            latency: durationMs->Option.mapOr("", value => value->Float.toString),
            firstEvent: seen === 0 ? "true" : "false",
            paymentMethod: paymentMethod->Option.mapOr("", SharedLoggerPaymentMethod.qualifiedName),
          },
          ~backfill=!context.scoped,
          ~pmSessionId=context.pmSessionId,
          ~sessionIdAtEnqueue=context.scoped ? context.sessionId : platform.getContext().sessionId,
        )
      }
    })
  }
