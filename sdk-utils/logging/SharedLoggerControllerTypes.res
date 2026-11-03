module type Utils = {
  let snakeCase: string => string
  let screamingSnakeCase: string => string
  let variantConstructor: 'a => string
  let variantName: 'a => string
  let spec: (
    'a,
    ~action: SharedLoggerTypes.action=?,
    ~outcome: SharedLoggerTypes.outcome=?,
  ) => SharedLoggerTypes.eventSpec
  let eventName: (
    ~category: SharedLoggerTypes.category,
    ~action: SharedLoggerTypes.action,
    ~subject: string,
    ~outcome: option<SharedLoggerTypes.outcome>,
  ) => string
  let truncateTo: (string, int) => string
  let truncate: string => string
  let sanitizeUrl: string => string
  let redact: string => string
  let safeRun: (unit => unit) => unit
  let jsonKind: JSON.t => int
  let normalize: (string, JSON.t) => JSON.t
  let normalizeDetails: SharedLoggerTypes.details => SharedLoggerTypes.details
  let eventDetails: 'a => SharedLoggerTypes.details
  let maskAll: string => bool
  let maskNone: string => bool
  let redactedValue: string => JSON.t
  let isFunction: JSON.t => bool
  let configSnapshot: (
    JSON.t,
    ~isSensitive: string => bool,
    ~maxBytes: int=?,
  ) => SharedLoggerTypes.details
  let mergeDetails: (
    ~data: SharedLoggerTypes.details,
    ~details: SharedLoggerTypes.details,
  ) => SharedLoggerTypes.details
  let fitToBudget: SharedLoggerTypes.details => SharedLoggerTypes.details
  let collectFields: (JSON.t, ~prefix: string, ~into: array<string>) => unit
  let firstString: (Dict.t<JSON.t>, array<string>) => option<string>
  let summary: (
    ~name: string,
    ~message: string=?,
    ~details: SharedLoggerTypes.details=?,
  ) => SharedLoggerTypes.errorSummary
  let stringDetails: array<('a, option<string>)> => array<('a, JSON.t)>
  let errorDetails: SharedLoggerTypes.errorSummary => array<(string, JSON.t)>
  let isAborted: SharedLoggerTypes.errorSummary => bool
  let summarizeErrorResponse: 'a => option<SharedLoggerTypes.errorSummary>
  let httpDetails: Fetch.Response.t => array<(string, JSON.t)>
  let httpError: Fetch.Response.t => SharedLoggerTypes.errorSummary
  let httpFailure: Fetch.Response.t => option<SharedLoggerTypes.errorSummary>
  let httpBodyFailure: ((Fetch.Response.t, 'a)) => option<SharedLoggerTypes.errorSummary>
  let httpBodyDetails: ((Fetch.Response.t, 'a)) => array<(string, JSON.t)>
  let intentErrorDetails: 'a => array<(string, JSON.t)>
  let intentResponseDetails: ((Fetch.Response.t, 'a)) => array<(string, JSON.t)>
  let payloadDetails: string => array<(string, JSON.t)>
  let summarizeValue: 'a => SharedLoggerTypes.errorSummary
  let namedField: string => string
  let utf8Length: string => int
  let summarizeExn: exn => SharedLoggerTypes.errorSummary
  let summarizeUnknown: 'a => SharedLoggerTypes.errorSummary
}
module type NativeRuntime = {
  type sourceType
  type resourceType
  type paymentMethodType
  type contextType = SharedLoggerPlatform.NativeTypes.context
  let configure: (~source: sourceType, ~version: string, ~appId: string, ~userAgent: string) => unit
  let isMerchantWindow: unit => bool
  let isNestedElement: unit => bool
  let isEnabled: SharedLoggerTypes.severity => bool
  let emitCounts: ref<Dict.t<int>>
  let onEmit: ref<unit => unit>
  let emit: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.severity,
    ~data: SharedLoggerTypes.details=?,
    ~details: SharedLoggerTypes.details=?,
    ~verbatim: SharedLoggerTypes.details=?,
    ~exn: exn=?,
    ~failure: 'response=?,
    ~durationMs: float=?,
    ~paymentMethod: paymentMethodType=?,
    ~context: contextType=?,
    ~source: sourceType=?,
    ~limit: int=?,
    ~message: string=?,
    ~once: bool=?,
    ~dedupKey: string=?,
  ) => unit
  let emitPhase: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~outcome: SharedLoggerTypes.outcome,
    ~data: array<(string, JSON.t)>=?,
    ~details: SharedLoggerTypes.details=?,
    ~startedAt: float=?,
    ~exn: 'a=?,
    ~paymentMethod: paymentMethodType=?,
    ~message: string=?,
    ~context: contextType=?,
    ~source: sourceType=?,
  ) => unit
  let observe: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~data: SharedLoggerTypes.details=?,
    ~details: SharedLoggerTypes.details=?,
    ~timeoutMs: int=?,
    ~failureOf: 'result => option<SharedLoggerTypes.errorSummary>=?,
    ~detailsOf: 'result => SharedLoggerTypes.details=?,
    ~paymentMethod: paymentMethodType=?,
    ~source: sourceType=?,
    ~syncOutcome: SharedLoggerTypes.outcome=?,
    ~message: string=?,
    ~onStep: (SharedLoggerTypes.step, SharedLoggerTypes.details) => unit=?,
    ~call: unit => 'value,
    ~context: contextType=?,
  ) => 'value
  let observeCallback: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~data: SharedLoggerTypes.details=?,
    ~details: SharedLoggerTypes.details=?,
    ~timeoutMs: int=?,
    ~failureOf: 'a => option<SharedLoggerTypes.errorSummary>=?,
    ~paymentMethod: paymentMethodType=?,
    ~source: sourceType=?,
    ~message: string=?,
    ~callback: 'fn,
    ~context: contextType=?,
  ) => 'fn
  let observeResource: (
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~url: string,
    ~resource: resourceType,
    ~attributes: array<(string, string)>=?,
    ~matchQuery: bool=?,
    ~paymentMethod: paymentMethodType=?,
    ~context: contextType=?,
    ~source: sourceType=?,
    ~abandoned: unit => bool=?,
    ~message: string=?,
    ~onLoad: unit => unit=?,
    ~onError: exn => unit=?,
  ) => unit
}
module type NativeContext = {
  type t = SharedLoggerPlatform.NativeTypes.context
  let context: ref<t>
  let onSessionChange: ref<unit => unit>
  let current: unit => t
  let fillFrom: (t, t) => t
  let setSessionData: (
    ~sessionId: string=?,
    ~merchantId: string=?,
    ~paymentId: string=?,
    ~authenticationId: string=?,
    unit,
  ) => unit
  let setPaymentId: string => unit
  let setPaymentIdFromCredentials: (~clientSecret: string=?, ~sdkAuthorization: string=?) => unit
  let setAuthenticationId: string => unit
}
module type WebRuntime = {
  type sourceType
  type resourceType
  type paymentMethodType
  type contextType = SharedLoggerPlatform.WebTypes.context
  let configure: (~source: sourceType, ~version: string, ~appId: string, ~userAgent: string) => unit
  let isMerchantWindow: unit => bool
  let isNestedElement: unit => bool
  let isEnabled: SharedLoggerTypes.severity => bool
  let emitCounts: ref<Dict.t<int>>
  let onEmit: ref<unit => unit>
  let emit: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.severity,
    ~data: SharedLoggerTypes.details=?,
    ~details: SharedLoggerTypes.details=?,
    ~verbatim: SharedLoggerTypes.details=?,
    ~exn: exn=?,
    ~failure: 'response=?,
    ~durationMs: float=?,
    ~paymentMethod: paymentMethodType=?,
    ~context: contextType=?,
    ~source: sourceType=?,
    ~limit: int=?,
    ~message: string=?,
    ~once: bool=?,
    ~dedupKey: string=?,
  ) => unit
  let emitPhase: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~outcome: SharedLoggerTypes.outcome,
    ~data: array<(string, JSON.t)>=?,
    ~details: SharedLoggerTypes.details=?,
    ~startedAt: float=?,
    ~exn: 'a=?,
    ~paymentMethod: paymentMethodType=?,
    ~message: string=?,
    ~context: contextType=?,
    ~source: sourceType=?,
  ) => unit
  let observe: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~data: SharedLoggerTypes.details=?,
    ~details: SharedLoggerTypes.details=?,
    ~timeoutMs: int=?,
    ~failureOf: 'result => option<SharedLoggerTypes.errorSummary>=?,
    ~detailsOf: 'result => SharedLoggerTypes.details=?,
    ~paymentMethod: paymentMethodType=?,
    ~source: sourceType=?,
    ~syncOutcome: SharedLoggerTypes.outcome=?,
    ~message: string=?,
    ~onStep: (SharedLoggerTypes.step, SharedLoggerTypes.details) => unit=?,
    ~call: unit => 'value,
    ~context: contextType=?,
  ) => 'value
  let observeCallback: (
    ~category: SharedLoggerTypes.category,
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~data: SharedLoggerTypes.details=?,
    ~details: SharedLoggerTypes.details=?,
    ~timeoutMs: int=?,
    ~failureOf: 'a => option<SharedLoggerTypes.errorSummary>=?,
    ~paymentMethod: paymentMethodType=?,
    ~source: sourceType=?,
    ~message: string=?,
    ~callback: 'fn,
    ~context: contextType=?,
  ) => 'fn
  let observeResource: (
    ~spec: SharedLoggerTypes.eventSpec,
    ~severity: SharedLoggerTypes.operationSeverity,
    ~url: string,
    ~resource: resourceType,
    ~attributes: array<(string, string)>=?,
    ~matchQuery: bool=?,
    ~paymentMethod: paymentMethodType=?,
    ~context: contextType=?,
    ~source: sourceType=?,
    ~abandoned: unit => bool=?,
    ~message: string=?,
    ~onLoad: unit => unit=?,
    ~onError: exn => unit=?,
  ) => unit
}
module type WebContext = {
  type t = SharedLoggerPlatform.WebTypes.context
  let context: ref<t>
  let onSessionChange: ref<unit => unit>
  let current: unit => t
  let fillFrom: (t, t) => t
  let setSessionData: (
    ~sessionId: string=?,
    ~merchantId: string=?,
    ~paymentId: string=?,
    ~pmSessionId: string=?,
    ~authenticationId: string=?,
    unit,
  ) => unit
  let setPaymentId: string => unit
  let setPaymentIdFromCredentials: (~clientSecret: string=?, ~sdkAuthorization: string=?) => unit
  let setAuthenticationId: string => unit
  let mayCarryContext: 'data => bool
  let startSessionFromMessage: Dict.t<JSON.t> => unit
}
type credentials = {sdkAuthorization: string, clientSecret: string}
