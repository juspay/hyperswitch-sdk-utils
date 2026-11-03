open PaymentMessageTypes
open CommonUtils

/* This decodes the bridge JSON. Native adapters own merchant-facing builders,
 interpolation, generated element keys, and callback registration. */
let warn = message => Console.warn("[Hyperswitch] " ++ message)

let readField = (dict, key, decode, path) =>
  dict
  ->Dict.get(key)
  ->Option.flatMap(json => {
    let value = decode(json)
    if value->Option.isNone {
      warn(path ++ "." ++ key ++ " has an invalid value; it will be ignored.")
    }
    value
  })

let readEnum = (json, values: array<(string, 'a)>) =>
  json
  ->JSON.Decode.string
  ->Option.flatMap(str => values->Array.find(((name, _)) => name == str))
  ->Option.map(((_, value)) => value)

let supportedStyleKeys = ["color", "fontSize", "fontWeight", "fontStyle", "textDecoration"]

let readStyle = (json, path): elementStyle =>
  switch json->JSON.Decode.object {
  | None =>
    warn(path ++ " must be an object; it will be ignored.")
    {}
  | Some(dict) =>
    dict
    ->Dict.keysToArray
    ->Array.forEach(key =>
      if !(supportedStyleKeys->Array.includes(key)) {
        warn(path ++ "." ++ key ++ " is not a supported style property; it will be ignored.")
      }
    )
    {
      color: ?readField(
        dict,
        "color",
        json =>
          json
          ->JSON.Decode.string
          ->Option.filter(color =>
            %re("/^#(?:[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$/")->RegExp.test(color)
          ),
        path,
      ),
      fontSize: ?readField(
        dict,
        "fontSize",
        json => json->JSON.Decode.float->Option.filter(size => size >= 0. && Float.isFinite(size)),
        path,
      ),
      fontWeight: ?readField(
        dict,
        "fontWeight",
        readEnum(_, [("400", #400), ("500", #500), ("600", #600), ("700", #700)]),
        path,
      ),
      fontStyle: ?readField(
        dict,
        "fontStyle",
        readEnum(_, [("normal", #normal), ("italic", #italic)]),
        path,
      ),
      textDecoration: ?readField(
        dict,
        "textDecoration",
        readEnum(_, [("none", #none), ("underline", #underline)]),
        path,
      ),
    }
  }

let readOptionalStyle = (dict, key, path): elementStyle =>
  dict->Dict.get(key)->Option.mapOr(({}: elementStyle), json => readStyle(json, path ++ "." ++ key))

let readElement = (json, path): element => {
  let dict = json->getDictFromJson
  switch dict->getOptionString("label") {
  | Some(label) =>
    Action({
      label,
      style: dict->readOptionalStyle("style", path),
    })
  | None =>
    warn(path ++ ".label is missing; showing the placeholder literally.")
    Invalid
  }
}

let readElements = (dict, path): Dict.t<element> => {
  let entries = []
  let seen = []
  let duplicates = []
  switch dict->Dict.get("action") {
  | None => ()
  | Some(json) =>
    switch json->JSON.Decode.array {
    | None => warn(path ++ ".action must be an array; it will be ignored.")
    | Some(items) =>
      items->Array.forEach(item => {
        let key = item->getDictFromJson->getString("key", "")
        if !(%re("/^[a-zA-Z0-9_]+$/")->RegExp.test(key)) {
          warn(path ++ ".action has an invalid key; it will be ignored.")
        } else if seen->Array.includes(key) {
          duplicates->Array.push(key)
          warn(path ++ " contains duplicate key " ++ key ++ "; its placeholder will stay literal.")
        } else {
          seen->Array.push(key)
          entries->Array.push((key, readElement(item, path ++ ".action." ++ key)))
        }
      })
    }
  }
  entries->Array.filter(((key, _)) => !(duplicates->Array.includes(key)))->Dict.fromArray
}

let defaultMessage: message = {
  displayMode: #default_sdk_message,
  value: None,
  elementStyles: {action: {}},
  elements: Dict.make(),
}

let readMessage = (json, path): option<message> =>
  switch json->JSON.Classify.classify {
  | Null => None
  | Object(dict) =>
    let value = dict->getOptionString("value")
    let displayMode = switch dict->getOptionString("displayMode") {
    | Some("custom_message") => #custom_message
    | Some("hidden") => #hidden
    | Some("default_sdk_message") => #default_sdk_message
    | None => value->Option.isSome ? #custom_message : #default_sdk_message
    | Some(_) =>
      warn(path ++ ".displayMode is invalid; the SDK default will be used.")
      #default_sdk_message
    }
    if displayMode !== #custom_message {
      Some({...defaultMessage, displayMode})
    } else {
      if value->Option.isNone {
        warn(path ++ ".value is missing; no custom message will be shown.")
      }
      Some({
        displayMode,
        value,
        elementStyles: {
          action: dict
          ->getDictFromDict("elementStyles")
          ->readOptionalStyle("action", path ++ ".elementStyles"),
        },
        elements: dict->getDictFromDict("elements")->readElements(path ++ ".elements"),
      })
    }
  | _ =>
    warn(path ++ " must be a message object such as {value: \"...\"}; it will be ignored.")
    None
  }

let readOptionalMessage = (dict, path) =>
  dict->Dict.get("message")->Option.flatMap(json => readMessage(json, path ++ ".message"))

let readConfigs = (config: Dict.t<JSON.t>): array<paymentMethodConfig> =>
  config
  ->getArray("paymentMethodsConfig")
  ->Array.filterMap(json =>
    json
    ->JSON.Decode.object
    ->Option.map(dict => {
      let paymentMethod = dict->getString("paymentMethod", "")
      let path = "paymentMethodsConfig." ++ paymentMethod
      let paymentMethodTypes =
        dict
        ->getArray("paymentMethodTypes")
        ->Array.filterMap(
          json =>
            json
            ->JSON.Decode.object
            ->Option.map(
              typeDict => {
                let paymentMethodType = typeDict->getString("paymentMethodType", "")
                {
                  paymentMethodType,
                  message: typeDict->readOptionalMessage(path ++ "." ++ paymentMethodType),
                }
              },
            ),
        )
      /* Cards take one message for credit and debit alike, as on web. */
      let paymentMethodTypes = if paymentMethod == "card" && paymentMethodTypes->Array.length > 0 {
        warn(path ++ ".paymentMethodTypes is not supported for cards; it will be ignored.")
        []
      } else {
        paymentMethodTypes
      }
      {
        paymentMethod,
        message: dict->readOptionalMessage(path),
        paymentMethodTypes,
      }
    })
  )

/* A payment method type's message wins over its payment method's message. */
let resolve = (configs: array<paymentMethodConfig>, ~paymentMethod, ~paymentMethodType) =>
  configs
  ->Array.find(config => config.paymentMethod == paymentMethod)
  ->Option.flatMap(config =>
    config.paymentMethodTypes
    ->Array.find(typeConfig => typeConfig.paymentMethodType == paymentMethodType)
    ->Option.flatMap(typeConfig => typeConfig.message)
    ->Option.orElse(config.message)
  )

/* The element's own style wins per property over the message's elementStyles.action;
 the renderer applies the theme for whatever is still unset. */
let mergeStyles = (base: elementStyle, override: elementStyle): elementStyle => {
  color: ?override.color->Option.orElse(base.color),
  fontSize: ?override.fontSize->Option.orElse(base.fontSize),
  fontWeight: ?override.fontWeight->Option.orElse(base.fontWeight),
  fontStyle: ?override.fontStyle->Option.orElse(base.fontStyle),
  textDecoration: ?override.textDecoration->Option.orElse(base.textDecoration),
}

type interactiveText = {key: string, label: string, style: elementStyle}
type segment =
  | Text(string)
  | CallbackAction(interactiveText)
type content = {segments: array<segment>, warnings: array<string>}

let renderContent = (message: message, ~subscribed): content => {
  let segments = []
  let warnings = []
  let addWarning = warning =>
    if !(warnings->Array.includes(warning)) {
      warnings->Array.push(warning)
    }
  if message.displayMode === #custom_message {
    let elementKeys = message.elements->Dict.keysToArray
    let usedKeys = []
    /* Splitting on a capturing regex alternates literal text (even) and placeholder keys (odd). */
    message.value
    ->Option.getOr("")
    ->String.splitByRegExp(%re("/\{\{([a-zA-Z0-9_]+)\}\}/"))
    ->Array.forEachWithIndex((part, index) => {
      let part = part->Option.getOr("")
      if mod(index, 2) == 0 {
        if part != "" {
          segments->Array.push(Text(part))
        }
      } else {
        let literal = "{{" ++ part ++ "}}"
        usedKeys->Array.push(part)
        /* The includes check keeps keys such as "constructor" off the object prototype. */
        let element = elementKeys->Array.includes(part) ? message.elements->Dict.get(part) : None
        segments->Array.push(
          switch element {
          | None =>
            addWarning("No element matches " ++ literal ++ "; showing the placeholder literally.")
            Text(literal)
          | Some(Invalid) => Text(literal)
          | Some(Action({label, style})) =>
            if subscribed {
              CallbackAction({
                key: part,
                label,
                style: mergeStyles(message.elementStyles.action, style),
              })
            } else {
              addWarning("Subscribe to customMessageElementClicked to enable message actions.")
              Text(label)
            }
          },
        )
      }
    })
    elementKeys->Array.forEach(key =>
      if !(usedKeys->Array.includes(key)) {
        addWarning("Message element " ++ key ++ " is unused; it will be ignored.")
      }
    )
  }
  {segments, warnings}
}
