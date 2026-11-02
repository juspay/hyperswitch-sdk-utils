include HyperLoaderLoggerEvents

let observeMerchantCall = (~event: merchantCallEvent, ~run) =>
  SharedLoggerProducer.observeMerchantCall(~event, ~severity=event->merchantCallSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let observeMerchantCallback = (~event: merchantCallbackEvent, ~run) =>
  SharedLoggerProducer.observeMerchantCallback(~event, ~severity=event->merchantCallbackSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~run)

let logMerchantCall = (~event: merchantCallEvent, ~emit) =>
  SharedLoggerProducer.logMerchantCall(
    ~event, ~severity=(event->merchantCallSeverity).success,
    ~dataOf=SharedLoggerRuntime.eventDetails,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      emit(~category, ~spec={...spec, outcome: Some(SharedLoggerTypes.Returned)}, ~severity, ~data),
  )

let logMerchantProps = (~event: merchantPropEvent, ~emit) =>
  SharedLoggerProducer.logMerchantProps(~event, ~severity=event->merchantPropSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)

let logMerchantIssue = (~issue: merchantIssue, ~emit) =>
  SharedLoggerProducer.logMerchantIssue(~event=issue, ~severity=issue->merchantIssueSeverity, ~dataOf=SharedLoggerRuntime.eventDetails, ~emit)
