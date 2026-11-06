type displayMode = [#custom_message | #default_sdk_message | #hidden]

type elementStyle = {
  color?: string,
  fontSize?: float,
  /* Font values pass through as strings (web takes CSS); each renderer maps what it supports. */
  fontWeight?: string,
  fontStyle?: string,
  textDecoration?: string,
}

/* The message is sent as its pieces, so text is never parsed for placeholders. `index` is
 the segment's position in its message's `value` array, as the wrapper sent it. */
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
