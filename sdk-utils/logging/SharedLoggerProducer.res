open SharedLoggerTypes

let fact = (~event, ~category, ~severity, ~dataOf, ~emit) =>
  emit(~category, ~spec=event->SharedLoggerEvent.spec, ~severity, ~data=event->dataOf)

let phase = (~event, ~category, ~action, ~severity, ~dataOf, ~emit) =>
  emit(~category, ~spec=event->SharedLoggerEvent.spec(~action), ~severity, ~data=event->dataOf)

let observe = (~event, ~category, ~action, ~severity, ~dataOf, ~run) =>
  run(~category, ~spec=event->SharedLoggerEvent.spec(~action), ~severity, ~data=event->dataOf)

let logLifecycle = (~event, ~severity, ~dataOf, ~emit) =>
  fact(~event, ~category=Lifecycle, ~severity, ~dataOf, ~emit)

let logState = (~event, ~severity, ~dataOf, ~emit) =>
  fact(~event, ~category=State, ~severity, ~dataOf, ~emit)

let logUser = (~event, ~severity, ~dataOf, ~emit) =>
  fact(~event, ~category=User, ~severity, ~dataOf, ~emit)

let logCrash = (~event, ~severity, ~emit) =>
  emit(~category=Crash, ~spec=event->SharedLoggerEvent.spec, ~severity)

let logApi = (~event, ~severity, ~dataOf, ~emit) =>
  phase(~event, ~category=Api, ~action=Request, ~severity, ~dataOf, ~emit)

let logFunction = (~event, ~severity, ~dataOf, ~emit) =>
  phase(~event, ~category=Function, ~action=Call, ~severity, ~dataOf, ~emit)

let observeApi = (~event, ~severity, ~dataOf, ~run) =>
  observe(~event, ~category=Api, ~action=Request, ~severity, ~dataOf, ~run)

let observeFunction = (~event, ~severity, ~dataOf, ~run) =>
  observe(~event, ~category=Function, ~action=Call, ~severity, ~dataOf, ~run)

let observeFunctionCallback = (~event, ~severity, ~dataOf, ~run) =>
  observe(~event, ~category=Function, ~action=Callback, ~severity, ~dataOf, ~run)

let observeMerchantCall = (~event, ~severity, ~dataOf, ~run) =>
  observe(~event, ~category=Merchant, ~action=Call, ~severity, ~dataOf, ~run)

let observeMerchantCallback = (~event, ~severity, ~dataOf, ~run) =>
  observe(~event, ~category=Merchant, ~action=Callback, ~severity, ~dataOf, ~run)

let logMerchantCall = (~event, ~severity, ~dataOf, ~emit) =>
  phase(~event, ~category=Merchant, ~action=Call, ~severity, ~dataOf, ~emit)

let logMerchantProps = (~event, ~severity, ~dataOf, ~emit) =>
  phase(~event, ~category=Merchant, ~action=Prop, ~severity, ~dataOf, ~emit)

let logMerchantIssue = (~event, ~severity, ~dataOf, ~emit) =>
  phase(~event, ~category=Merchant, ~action=IntegrationIssue, ~severity, ~dataOf, ~emit)

let observeResource = (~event, ~severity, ~dataOf, ~run) =>
  observe(~event, ~category=Resource, ~action=Load, ~severity, ~dataOf, ~run)

let resource = (~event, ~severity, ~run) =>
  run(~spec=event->SharedLoggerEvent.spec(~action=Load), ~severity)
