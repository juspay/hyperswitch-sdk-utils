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

/* The message is sent as its pieces, so text is never parsed for placeholders. `index`
 counts tappable segments across the whole configuration in wire order (each payment
 method's message, then its types' messages), matching the wrapper's action list. */
type segment = Text(string) | Action({text: string, style: elementStyle, index: int})

type elementStyles = {action: elementStyle}
/* Unset fields are None, so a payment method type message can inherit them. */
type message = {
  displayMode: option<displayMode>,
  value: option<array<segment>>,
  elementStyles: elementStyles,
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

type elementClickedEvent = {index: int}
