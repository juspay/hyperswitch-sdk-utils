module Utils = SharedLoggerUtils.Make(SharedLoggerConfig.Native)
type update = {
  sessionId: option<string>,
  merchantId: option<string>,
  paymentId: option<string>,
  pmSessionId: option<string>,
  authenticationId: option<string>,
}
let keep = (existing, incoming) =>
  switch incoming->Option.map(String.trim) {
  | Some("") | None => existing
  | Some(value) => value
  }

module type Policy = {
  type t
  let empty: t
  let sessionId: t => string
  let withSessionId: (t, string) => t
  let update: (t, update) => t
  let fillFrom: (t, t) => t
  let changed: (t, t) => bool
}
module State = (
  Policy: Policy,
  Host: {
    let backfill: Policy.t => unit
  },
) => {
  type t = Policy.t
  let empty = Policy.empty
  let context = ref(empty)
  let onSessionChange = ref(() => ())
  let current = () => context.contents
  let fillFrom = Policy.fillFrom
  let setSessionData = (
    ~sessionId=?,
    ~merchantId=?,
    ~paymentId=?,
    ~pmSessionId=?,
    ~authenticationId=?,
    (),
  ) => {
    let previous = context.contents
    let base = switch sessionId->Option.map(String.trim) {
    | Some(sessionId) if sessionId !== "" && sessionId !== Policy.sessionId(previous) =>
      Policy.withSessionId(empty, sessionId)
    | _ => previous
    }
    let next = Policy.update(
      base,
      {sessionId, merchantId, paymentId, pmSessionId, authenticationId},
    )
    context := next
    if Policy.sessionId(next) !== Policy.sessionId(previous) {
      Utils.safeRun(() => onSessionChange.contents())
    }
    if Policy.changed(next, previous) {
      Utils.safeRun(() => Host.backfill(next))
    }
  }
}
module Credentials = (
  Host: {
    let decode: string => string
  },
) => {
  let paymentIdOfClientSecret = clientSecret =>
    switch clientSecret->String.split("_secret_") {
    | parts if parts->Array.length >= 2 => parts->Array.getUnsafe(0)
    | _ => ""
    }

  let sdkAuthorizationValue = (sdkAuthorization, key) =>
    try {
      let prefix = key ++ "="
      sdkAuthorization
      ->Host.decode
      ->String.split(",")
      ->Array.findMap(entry =>
        entry->String.startsWith(prefix)
          ? switch entry->String.sliceToEnd(~start=prefix->String.length) {
            | "" => None
            | value => Some(value)
            }
          : None
      )
    } catch {
    | _ => None
    }
}
module Native = (
  Host: {
    let decode: string => string
    let backfill: SharedLoggerPlatform.NativeTypes.context => unit
  },
) => {
  module Policy = {
    type t = SharedLoggerPlatform.NativeTypes.context
    let empty: t = {
      sdkInitAt: None,
      attemptId: "",
      sessionId: "",
      merchantId: "",
      paymentId: "",
      authenticationId: "",
    }

    let fillFrom = (started: t, latest: t): t =>
      started.sessionId !== "" && started.sessionId !== latest.sessionId
        ? started
        : {
            sdkInitAt: switch started.sdkInitAt {
            | Some(_) as started => started
            | None => latest.sdkInitAt
            },
            attemptId: started.attemptId,
            sessionId: latest.sessionId->keep(Some(started.sessionId)),
            merchantId: latest.merchantId->keep(Some(started.merchantId)),
            paymentId: latest.paymentId->keep(Some(started.paymentId)),
            authenticationId: latest.authenticationId->keep(Some(started.authenticationId)),
          }

    let sessionId = (value: t) => value.sessionId
    let withSessionId = (value: t, sessionId) => {...value, sessionId}
    let update = (base: t, values: update): t => {
      sdkInitAt: base.sdkInitAt,
      attemptId: base.attemptId,
      sessionId: base.sessionId->keep(values.sessionId),
      merchantId: base.merchantId->keep(values.merchantId),
      paymentId: base.paymentId->keep(values.paymentId),
      authenticationId: base.authenticationId->keep(values.authenticationId),
    }
    let changed = (next: t, previous: t) =>
      next.sessionId !== previous.sessionId ||
      next.merchantId !== previous.merchantId ||
      next.paymentId !== previous.paymentId ||
      next.authenticationId !== previous.authenticationId
  }
  module Engine = State(Policy, Host)
  include Engine
  include Credentials(Host)
  let setSessionData = (~sessionId=?, ~merchantId=?, ~paymentId=?, ~authenticationId=?, ()) =>
    Engine.setSessionData(~sessionId?, ~merchantId?, ~paymentId?, ~authenticationId?, ())
  let paymentIdOfSdkAuthorization = sdkAuthorization =>
    switch sdkAuthorization->sdkAuthorizationValue("payment_id") {
    | Some(_) as paymentId => paymentId
    | None => sdkAuthorization->sdkAuthorizationValue("payment_method_session_id")
    }

  let setPaymentId = paymentId =>
    switch paymentId->String.trim {
    | "" => ()
    | paymentId => setSessionData(~paymentId, ())
    }

  let setPaymentIdFromCredentials = (~clientSecret="", ~sdkAuthorization=?) =>
    sdkAuthorization
    ->Option.flatMap(paymentIdOfSdkAuthorization)
    ->Option.getOr(clientSecret->paymentIdOfClientSecret)
    ->setPaymentId

  let setAuthenticationId = authenticationId =>
    switch authenticationId->String.trim {
    | "" => ()
    | authenticationId => setSessionData(~authenticationId, ())
    }
}
module Web = (
  Host: {
    let decode: string => string
    let backfill: SharedLoggerPlatform.WebTypes.context => unit
  },
) => {
  module Policy = {
    type t = SharedLoggerPlatform.WebTypes.context
    let empty: t = {
      sessionId: "",
      merchantId: "",
      paymentId: "",
      pmSessionId: "",
      authenticationId: "",
      scoped: false,
    }

    let fillFrom = (started: t, latest: t): t =>
      started.scoped || (started.sessionId !== "" && started.sessionId !== latest.sessionId)
        ? started
        : {
            sessionId: latest.sessionId->keep(Some(started.sessionId)),
            merchantId: latest.merchantId->keep(Some(started.merchantId)),
            paymentId: latest.paymentId->keep(Some(started.paymentId)),
            pmSessionId: latest.pmSessionId->keep(Some(started.pmSessionId)),
            authenticationId: latest.authenticationId->keep(Some(started.authenticationId)),
            scoped: false,
          }

    let sessionId = (value: t) => value.sessionId
    let withSessionId = (value: t, sessionId) => {...value, sessionId}
    let update = (base: t, values: update): t => {
      sessionId: base.sessionId->keep(values.sessionId),
      merchantId: base.merchantId->keep(values.merchantId),
      paymentId: base.paymentId->keep(values.paymentId),
      pmSessionId: base.pmSessionId->keep(values.pmSessionId),
      authenticationId: base.authenticationId->keep(values.authenticationId),
      scoped: false,
    }
    let changed = (next: t, previous: t) =>
      next.sessionId !== previous.sessionId ||
      next.merchantId !== previous.merchantId ||
      next.paymentId !== previous.paymentId ||
      next.pmSessionId !== previous.pmSessionId ||
      next.authenticationId !== previous.authenticationId
  }
  module Engine = State(Policy, Host)
  include Engine
  include Credentials(Host)
  let paymentIdOfSdkAuthorization = sdkAuthorization =>
    sdkAuthorization->sdkAuthorizationValue("payment_id")

  let pmSessionIdOfSdkAuthorization = sdkAuthorization =>
    sdkAuthorization->sdkAuthorizationValue("payment_method_session_id")

  let setPaymentId = paymentId =>
    switch paymentId->String.trim {
    | "" => ()
    | paymentId => setSessionData(~paymentId, ())
    }

  let setPmSessionId = pmSessionId =>
    switch pmSessionId->String.trim {
    | "" => ()
    | pmSessionId => setSessionData(~pmSessionId, ())
    }

  let setPaymentIdFromCredentials = (~clientSecret="", ~sdkAuthorization=?) => {
    let paymentId = clientSecret->paymentIdOfClientSecret
    sdkAuthorization
    ->Option.flatMap(paymentIdOfSdkAuthorization)
    ->Option.getOr(paymentId)
    ->setPaymentId
    sdkAuthorization->Option.flatMap(pmSessionIdOfSdkAuthorization)->Option.forEach(setPmSessionId)
  }

  let setPaymentIdFromClientSecret = clientSecret =>
    clientSecret->paymentIdOfClientSecret->setPaymentId

  let setAuthenticationId = authenticationId =>
    switch authenticationId->String.trim {
    | "" => ()
    | authenticationId => setSessionData(~authenticationId, ())
    }

  let mayCarryContext: 'data => bool = %raw(`
  function (d) {
    return typeof d == "string" &&
      /"(?:sdkSessionId|publishableKey|sdkAuthorization|paymentId|clientSecret|pmSessionId|authenticationId)"\s*:/.test(d);
  }
`)

  let startSessionFromMessage = message =>
    Utils.safeRun(() => {
      let nested = key =>
        message->Dict.get(key)->Option.flatMap(JSON.Decode.object)->Option.getOr(Dict.make())
      let sources = [
        message,
        nested("loggerContext"),
        nested("metadata"),
        nested("paymentOptions"),
        nested("options"),
      ]
      let readField = key =>
        sources
        ->Array.findMap(source =>
          switch source
          ->Dict.get(key)
          ->Option.flatMap(JSON.Decode.string)
          ->Option.map(String.trim) {
          | Some("") | None => None
          | value => value
          }
        )
        ->Option.getOr("")
      let sdkAuthorization = readField("sdkAuthorization")
      let paymentId = switch sdkAuthorization->paymentIdOfSdkAuthorization {
      | Some(paymentId) => paymentId
      | None =>
        switch readField("paymentId") {
        | "" => readField("clientSecret")->paymentIdOfClientSecret
        | paymentId => paymentId
        }
      }
      let pmSessionId =
        sdkAuthorization
        ->pmSessionIdOfSdkAuthorization
        ->Option.getOr(readField("pmSessionId"))
      setSessionData(
        ~sessionId=readField("sdkSessionId"),
        ~merchantId=readField("publishableKey"),
        ~paymentId,
        ~pmSessionId,
        ~authenticationId=readField("authenticationId"),
        (),
      )
    })

  let sharedContext = (~context=context.contents, ()) => {
    let {sessionId, merchantId, paymentId, pmSessionId, authenticationId} = context
    (
      "loggerContext",
      [
        ("sdkSessionId", sessionId->JSON.Encode.string),
        ("publishableKey", merchantId->JSON.Encode.string),
        ("paymentId", paymentId->JSON.Encode.string),
        ("pmSessionId", pmSessionId->JSON.Encode.string),
        ("authenticationId", authenticationId->JSON.Encode.string),
      ]
      ->Dict.fromArray
      ->JSON.Encode.object,
    )
  }
}
