open PaymentMessageTypes
open CommonUtils

/* This decodes the bridge JSON. The wrappers own the merchant-facing API and callbacks. */
let readField = (dict, key, decode) => dict->Dict.get(key)->Option.flatMap(decode)

let readEnum = (json, values: array<(string, 'a)>) =>
  json
  ->JSON.Decode.string
  ->Option.flatMap(str => values->Array.find(((name, _)) => name == str))
  ->Option.map(((_, value)) => value)

let readStyle = (json): elementStyle =>
  switch json->JSON.Decode.object {
  | None => {}
  | Some(dict) => {
      color: ?readField(dict, "color", json =>
        json
        ->JSON.Decode.string
        ->Option.filter(color =>
          %re("/^#(?:[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$/")->RegExp.test(color)
        )
      ),
      fontSize: ?readField(dict, "fontSize", json =>
        json->JSON.Decode.float->Option.filter(size => size >= 0. && Float.isFinite(size))
      ),
      fontWeight: ?readField(dict, "fontWeight", JSON.Decode.string),
      fontStyle: ?readField(
        dict,
        "fontStyle",
        readEnum(_, [("normal", #normal), ("italic", #italic)]),
      ),
      textDecoration: ?readField(
        dict,
        "textDecoration",
        readEnum(_, [("none", #none), ("underline", #underline)]),
      ),
    }
  }

let readOptionalStyle = (dict, key): elementStyle =>
  dict->Dict.get(key)->Option.mapOr(({}: elementStyle), readStyle)

/* Tappable entries are counted even when malformed or not shown, so every later index
 still matches the wrapper's action list. */
let readSegments = (items: array<JSON.t>, ~actions: ref<int>): array<segment> =>
  items->Array.filterMap(item => {
    let dict = item->getDictFromJson
    let text = dict->getOptionString("text")
    if dict->getBool("tappable", false) {
      let index = actions.contents
      actions := index + 1
      text->Option.map(text => Action({text, style: dict->readOptionalStyle("style"), index}))
    } else {
      text->Option.map(text => Text(text))
    }
  })

let readMessage = (json, ~actions): option<message> =>
  switch json->JSON.Classify.classify {
  | Object(dict) =>
    Some({
      displayMode: switch dict->getOptionString("displayMode") {
      | Some("custom_message") => Some(#custom_message)
      | Some("hidden") => Some(#hidden)
      | Some("default_sdk_message") => Some(#default_sdk_message)
      | _ => None
      },
      value: dict
      ->Dict.get("value")
      ->Option.flatMap(JSON.Decode.array)
      ->Option.map(readSegments(_, ~actions)),
      elementStyles: {
        action: dict
        ->getDictFromDict("elementStyles")
        ->readOptionalStyle("action"),
      },
    })
  | _ => None
  }

let readOptionalMessage = (dict, ~actions) =>
  dict->Dict.get("message")->Option.flatMap(readMessage(_, ~actions))

let readConfigs = (config: Dict.t<JSON.t>): array<paymentMethodConfig> => {
  /* One running count over every message, in the order the wrappers list their actions. */
  let actions = ref(0)
  config
  ->getArray("paymentMethodsConfig")
  ->Array.filterMap(json =>
    json
    ->JSON.Decode.object
    ->Option.map(dict => {
      let paymentMethod = dict->getString("paymentMethod", "")
      let message = dict->readOptionalMessage(~actions)
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
                  message: typeDict->readOptionalMessage(~actions),
                }
              },
            ),
        )
      /* Cards take one message for credit and debit alike, as on web. */
      let paymentMethodTypes = paymentMethod == "card" ? [] : paymentMethodTypes
      {
        paymentMethod,
        message,
        paymentMethodTypes,
      }
    })
  )
}

/* The element's own style wins per property over the message's elementStyles.action;
 the renderer applies the theme for whatever is still unset. */
let mergeStyles = (base: elementStyle, override: elementStyle): elementStyle => {
  color: ?override.color->Option.orElse(base.color),
  fontSize: ?override.fontSize->Option.orElse(base.fontSize),
  fontWeight: ?override.fontWeight->Option.orElse(base.fontWeight),
  fontStyle: ?override.fontStyle->Option.orElse(base.fontStyle),
  textDecoration: ?override.textDecoration->Option.orElse(base.textDecoration),
}

/* Without an explicit display mode, a message with text is custom, as on web. */
let displayModeOf = (message: message) =>
  message.displayMode->Option.getOr(
    message.value->Option.isSome ? #custom_message : #default_sdk_message,
  )

/* A payment method type's message wins over its payment method's message, field by field:
 whatever the type leaves unset (text, display mode, each action style property) comes from
 the payment method's message. Text set on the type makes it custom unless it says otherwise. */
let resolve = (configs: array<paymentMethodConfig>, ~paymentMethod, ~paymentMethodType) =>
  configs
  ->Array.find(config => config.paymentMethod == paymentMethod)
  ->Option.flatMap(config =>
    switch (
      config.paymentMethodTypes
      ->Array.find(typeConfig => typeConfig.paymentMethodType == paymentMethodType)
      ->Option.flatMap(typeConfig => typeConfig.message),
      config.message,
    ) {
    | (Some(typeMessage), Some(method)) =>
      Some({
        displayMode: typeMessage.displayMode->Option.orElse(
          typeMessage.value->Option.isSome ? Some(#custom_message) : method.displayMode,
        ),
        value: typeMessage.value->Option.orElse(method.value),
        elementStyles: {
          action: mergeStyles(method.elementStyles.action, typeMessage.elementStyles.action),
        },
      })
    | (Some(typeMessage), None) => Some(typeMessage)
    | (None, method) => method
    }
  )
