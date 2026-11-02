open SharedLoggerTypes
include HyperLoaderLoggerEvents

let observeMerchantCall = (
  ~event: merchantCallEvent,
  ~details=?,
  ~timeoutMs=?,
  ~failureOf=SharedLoggerRuntime.summarizeErrorResponse,
  ~detailsOf=?,
  ~source=?,
  ~context=?,
  ~message=?,
  ~call,
) =>
  SharedLoggerProducer.observeMerchantCall(
    ~event, ~severity=event->merchantCallSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observe(~category, ~spec, ~severity, ~data,
        ~details?, ~timeoutMs?, ~failureOf, ~detailsOf?, ~source?, ~context?, ~message?, ~call),
  )

let observeMerchantCallback = (
  ~event: merchantCallbackEvent,
  ~context=?,
  ~source=?,
  ~timeoutMs=?,
  ~message=?,
  ~callback,
) =>
  SharedLoggerProducer.observeMerchantCallback(
    ~event, ~severity=event->merchantCallbackSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~run=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.observeCallback(~category, ~spec, ~severity, ~data,
        ~context?, ~source?, ~timeoutMs?,
        ~failureOf=SharedLoggerRuntime.summarizeErrorResponse, ~message?, ~callback),
  )

let logMerchantCall = (~event: merchantCallEvent, ~details=?, ~message=?) =>
  SharedLoggerProducer.logMerchantCall(
    ~event, ~severity=(event->merchantCallSeverity).success, ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      SharedLoggerRuntime.emit(~category, ~spec={...spec, outcome: Some(Returned)},
        ~severity, ~data, ~details?, ~message?),
  )

let logMerchantProps = (
  ~event: merchantPropEvent,
  ~details=?,
  ~config: option<JSON.t>=?,
  ~isSensitive=SharedLoggerFormatting.maskAll,
  ~message=?,
) =>
  if !SharedLoggerRuntime.isNestedElement() && event->merchantPropSeverity->SharedLoggerRuntime.isEnabled {
    SharedLoggerProducer.logMerchantProps(
      ~event, ~severity=event->merchantPropSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
      ~emit=(~category, ~spec, ~severity, ~data) =>
        SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data, ~details?,
          ~verbatim=?config->Option.map(config => config->SharedLoggerFormatting.configSnapshot(~isSensitive)),
          ~message?, ~once=true),
    )
  }

let logMerchantIssue = (~issue: merchantIssue, ~details=?, ~context=?, ~message=?) =>
  if !SharedLoggerRuntime.isNestedElement() {
    SharedLoggerProducer.logMerchantIssue(
      ~event=issue, ~severity=issue->merchantIssueSeverity, ~dataOf=SharedLoggerRuntime.eventDetails,
      ~emit=(~category, ~spec, ~severity, ~data) =>
        SharedLoggerRuntime.emit(~category, ~spec, ~severity, ~data,
          ~context?, ~details?, ~message?, ~once=true),
    )
  }

let startSession = (~sessionId=?, ~merchantId=?) => {
  SharedLoggerRuntime.setSession(~sessionId?, ~merchantId?, ())
}
