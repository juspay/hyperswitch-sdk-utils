open PaymentMessageTypes
open CommonUtils

/* This decodes the bridge JSON. The wrappers own the merchant-facing API and callbacks. */
let readField = (dict, key, decode) => dict->Dict.get(key)->Option.flatMap(decode)

let readStyle = (json): elementStyle =>
  switch json->JSON.Decode.object {
  | None => {}
  | Some(dict) => {
      color: ?readField(dict, "color", JSON.Decode.string),
      fontSize: ?readField(dict, "fontSize", json =>
        json->JSON.Decode.float->Option.filter(size => size >= 0. && Float.isFinite(size))
      ),
      fontWeight: ?readField(dict, "fontWeight", JSON.Decode.string),
      fontStyle: ?readField(dict, "fontStyle", JSON.Decode.string),
      textDecoration: ?readField(dict, "textDecoration", JSON.Decode.string),
    }
  }

let readOptionalStyle = (dict, key): elementStyle =>
  dict->Dict.get(key)->Option.mapOr(({}: elementStyle), readStyle)

/* An action keeps its position in the wire array, so a tap names it directly even when
 malformed entries before it are dropped. */
let readSegments = (items: array<JSON.t>): array<segment> =>
  items
  ->Array.mapWithIndex((item, index) => {
    let dict = item->getDictFromJson
    dict
    ->getOptionString("text")
    ->Option.map(text =>
      dict->getBool("tappable", false)
        ? Action({text, style: dict->readOptionalStyle("style"), index})
        : Text(text)
    )
  })
  ->Array.keepSome

let readMessage = (json): option<message> =>
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
      ->Option.map(readSegments),
      elementStyles: {
        action: dict
        ->getDictFromDict("elementStyles")
        ->readOptionalStyle("action"),
      },
    })
  | _ => None
  }

let readOptionalMessage = dict => dict->Dict.get("message")->Option.flatMap(readMessage)

let readConfigs = (config: Dict.t<JSON.t>): array<paymentMethodConfig> => {
  config
  ->getArray("paymentMethodsConfig")
  ->Array.filterMap(json =>
    json
    ->JSON.Decode.object
    ->Option.map(dict => {
      let paymentMethod = dict->getString("paymentMethod", "")
      let message = dict->readOptionalMessage
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
                  message: typeDict->readOptionalMessage,
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
 the payment method's message. Text set on the type makes it custom unless it says otherwise.
 Also returns the payment method type a tap reports: the type when the shown text is the type
 message's own, otherwise "" (the text, and so its actions, belong to the method's message). */
let resolve = (configs: array<paymentMethodConfig>, ~paymentMethod, ~paymentMethodType): option<(
  message,
  string,
)> =>
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
      Some((
        {
          displayMode: typeMessage.displayMode->Option.orElse(
            typeMessage.value->Option.isSome ? Some(#custom_message) : method.displayMode,
          ),
          value: typeMessage.value->Option.orElse(method.value),
          elementStyles: {
            action: mergeStyles(method.elementStyles.action, typeMessage.elementStyles.action),
          },
        },
        typeMessage.value->Option.isSome ? paymentMethodType : "",
      ))
    | (Some(typeMessage), None) => Some((typeMessage, paymentMethodType))
    | (None, method) => method->Option.map(method => (method, ""))
    }
  )
