type displayMode = [#custom_message | #default_sdk_message | #hidden]
type fontStyle = [#normal | #italic]
type textDecoration = [#none | #underline]

type elementStyle = {
  color?: string,
  fontSize?: float,
  /* Platform-specific weight names pass through; each renderer maps what it supports. */
  fontWeight?: string,
  fontStyle?: fontStyle,
  textDecoration?: textDecoration,
}

type action = {label: string, style: elementStyle}
type element = Action(action) | Invalid

type elementStyles = {action: elementStyle}
type message = {
  displayMode: displayMode,
  value: option<string>,
  elementStyles: elementStyles,
  elements: Dict.t<element>,
}

type paymentMethodTypeConfig = {
  paymentMethodType: string,
  message: option<message>,
}

type paymentMethodConfig = {
  paymentMethod: string,
  message: option<message>,
  paymentMethodTypes: array<paymentMethodTypeConfig>,
}

type elementClickedEvent = {
  key: string,
  \"type": string,
  paymentMethod: string,
  paymentMethodType: string,
}
