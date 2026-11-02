include HyperLoaderLoggerEvents

let observeMerchantCall = (~event: merchantCallEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeMerchantCall(~event, ~severity=event->merchantCallSeverity, ~dataOf, ~run)

let observeMerchantCallback = (~event: merchantCallbackEvent, ~dataOf, ~run) =>
  SharedLoggerProducer.observeMerchantCallback(~event, ~severity=event->merchantCallbackSeverity, ~dataOf, ~run)

let logMerchantCall = (~event: merchantCallEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logMerchantCall(
    ~event, ~severity=(event->merchantCallSeverity).success, ~dataOf,
    ~emit=(~category, ~spec, ~severity, ~data) =>
      emit(~category, ~spec={...spec, outcome: Some(SharedLoggerTypes.Returned)}, ~severity, ~data),
  )

let logMerchantProps = (~event: merchantPropEvent, ~dataOf, ~emit) =>
  SharedLoggerProducer.logMerchantProps(~event, ~severity=event->merchantPropSeverity, ~dataOf, ~emit)

let logMerchantIssue = (~issue: merchantIssue, ~dataOf, ~emit) =>
  SharedLoggerProducer.logMerchantIssue(~event=issue, ~severity=issue->merchantIssueSeverity, ~dataOf, ~emit)
