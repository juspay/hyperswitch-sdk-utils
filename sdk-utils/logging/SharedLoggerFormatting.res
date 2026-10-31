open SharedLoggerTypes

external anyTypeToJson: 'value => JSON.t = "%identity"

let truncateTo = (value, limit) =>
  value->String.length > limit ? value->String.slice(~start=0, ~end=limit) : value

let sanitizeUrl = url => url->String.replaceRegExp(%re("/[?#].*$/"), "")

let redact = text =>
  text
  ->String.replaceRegExp(%re("/[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}/g"), "[email]")
  ->String.replaceRegExp(%re("/\d(?:[ -]?\d){5,}/g"), "[digits]")

let jsonKind: JSON.t => int = %raw(`
  (v) => {
    var t = Object.prototype.toString.call(v);
    return t == "[object Undefined]" ? 1 : t == "[object Array]" ? 2
      : t == "[object Null]" || t == "[object Number]" || t == "[object Boolean]" || t == "[object String]" ? 0 : 3;
  }
`)
external asArray: JSON.t => array<JSON.t> = "%identity"
external asDict: JSON.t => Dict.t<JSON.t> = "%identity"

let rec normalize = (key, value, ~maxTextLength) =>
  switch value->JSON.Decode.string {
  | Some(text) =>
    let text = if key === "" {
      text
    } else if key === "url" || key === "href" || key === "uri" || %re("/_(?:url|href|uri)$/")->RegExp.test(key) {
      text->sanitizeUrl
    } else if key === "message" || key->String.endsWith("_message") {
      text->redact
    } else if %re("/^[A-Z][A-Za-z0-9]*$/")->RegExp.test(text) {
      text->SharedLoggerEvent.screamingSnakeCase
    } else {
      text
    }
    text->truncateTo(maxTextLength)->JSON.Encode.string
  | None =>
    switch value->jsonKind {
    | 1 => JSON.Encode.null
    | 2 => value->asArray->Array.map(item => normalize("", item, ~maxTextLength))->JSON.Encode.array
    | 3 =>
      value
      ->asDict
      ->Dict.toArray
      ->Array.filterMap(((key, value)) =>
        value->jsonKind === 1
          ? None
          : {
              let key = key->SharedLoggerEvent.snakeCase
              Some((key, normalize(key, value, ~maxTextLength)))
            }
      )
      ->Dict.fromArray
      ->JSON.Encode.object
    | _ => value
    }
  }

let normalizeDetails = (entries: details, ~maxTextLength): details =>
  entries->Array.map(((key, value)) => {
    let key = key->SharedLoggerEvent.snakeCase
    (key, normalize(key, value, ~maxTextLength))
  })

let eventDetails = (event, ~maxTextLength): details =>
  event
  ->anyTypeToJson
  ->JSON.Decode.object
  ->Option.flatMap(object => object->Dict.get("_0"))
  ->Option.flatMap(payload => normalize("", payload, ~maxTextLength)->JSON.Decode.object)
  ->Option.mapOr([], Dict.toArray)

let mergeDetails = (~data: details, ~details: details): details => {
  let typedKeys = data->Array.map(((key, _)) => key)
  data->Array.concat(
    details->Array.map(((key, value)) =>
      typedKeys->Array.includes(key) ? ("detail_" ++ key, value) : (key, value)
    ),
  )
}

let fitToBudget = (entries: details, ~maxDetailBytes): details => {
  let sizeOf = (entries: details) =>
    entries->Dict.fromArray->JSON.Encode.object->JSON.stringify->String.length
  if entries->sizeOf <= maxDetailBytes {
    entries
  } else {
    let remaining = entries->Array.copy
    let dropped = []
    while remaining->Array.length > 0 && remaining->sizeOf > maxDetailBytes {
      let (largest, _) = remaining->Array.reduceWithIndex((0, -1), (
        (largest, largestSize),
        (_, value),
        index,
      ) => {
        let size = value->JSON.stringify->String.length
        size > largestSize ? (index, size) : (largest, largestSize)
      })
      remaining
      ->Array.get(largest)
      ->Option.forEach(((key, _)) => dropped->Array.push(key->JSON.Encode.string))
      remaining->Array.splice(~start=largest, ~remove=1, ~insert=[])
    }
    remaining->Array.concat([("dropped_details", dropped->JSON.Encode.array)])
  }
}

let summary = (~name, ~message=?, ~details=[]) => {name, message, details}

let firstString = (object, keys) =>
  keys->Array.findMap(key => object->Dict.get(key)->Option.flatMap(JSON.Decode.string))

let summarizeValue = value => {
  let json = value->anyTypeToJson
  switch JSON.Classify.classify(json) {
  | String(value) => summary(~name="THROWN_VALUE", ~message=value)
  | Object(object) =>
    summary(
      ~name=object
      ->firstString(["name", "code", "type", "reason", "statusCode"])
      ->Option.getOr("UNKNOWN_ERROR")
      ->SharedLoggerEvent.screamingSnakeCase,
      ~message=?object->firstString(["message", "description", "statusMessage"]),
    )
  | _ => summary(~name="UNKNOWN_ERROR")
  }
}

let summarizeExn = error =>
  error->summarizeValue

let summarizeUnknown = value => value->summarizeExn

let isAborted = summary => summary.name === "ABORT_ERROR"

let stringDetails = pairs =>
  pairs->Array.filterMap(((key, value)) =>
    value->Option.map(value => (key, value->JSON.Encode.string))
  )

let summarizeErrorResponse = result =>
  result
  ->anyTypeToJson
  ->JSON.Decode.object
  ->Option.flatMap(object => object->Dict.get("error"))
  ->Option.flatMap(json =>
    switch JSON.Classify.classify(json) {
    | Null => None
    | String(message) => Some(summary(~name="ERROR_RESPONSE", ~message))
    | Object(object) =>
      Some(
        summary(
          ~name=object
          ->firstString(["type", "code", "reason"])
          ->Option.getOr("ERROR_RESPONSE")
          ->SharedLoggerEvent.screamingSnakeCase,
          ~message=?object->firstString(["message"]),
          ~details=[
            ("error_code", object->firstString(["code"])),
            ("error_reason", object->firstString(["reason"])),
          ]->stringDetails,
        ),
      )
    | _ => Some(summary(~name="ERROR_RESPONSE"))
    }
  )

let errorDetails = summary =>
  [("error_type", summary.name->JSON.Encode.string)]
  ->Array.concat([("error_message", summary.message)]->stringDetails)
  ->Array.concat(summary.details)

let rec collectFields = (json, ~prefix, ~into) =>
  switch json->jsonKind {
  | 3 =>
    json
    ->asDict
    ->Dict.toArray
    ->Array.forEach(((key, value)) => {
      let path = prefix === "" ? key->SharedLoggerEvent.snakeCase : prefix ++ "." ++ key->SharedLoggerEvent.snakeCase
      value->jsonKind >= 2 ? value->collectFields(~prefix=path, ~into) : into->Array.push(path)
    })
  | 2 => json->asArray->Array.forEach(value => value->collectFields(~prefix=prefix ++ "[]", ~into))
  | _ => prefix === "" ? () : into->Array.push(prefix)
  }

let payloadDetails = (body, ~maxPayloadFields) =>
  switch body->JSON.parseExn {
  | json =>
    let into = []
    json->collectFields(~prefix="", ~into)
    switch into
    ->Set.fromArray
    ->Set.values
    ->Iterator.toArray
    ->Array.slice(~start=0, ~end=maxPayloadFields) {
    | [] => []
    | fields => [
        ("request_fields", fields->Array.map(JSON.Encode.string)->JSON.Encode.array),
        ("request_field_count", fields->Array.length->JSON.Encode.int),
      ]
    }
  | exception _ => []
  }
