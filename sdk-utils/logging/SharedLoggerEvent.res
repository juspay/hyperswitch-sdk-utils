open SharedLoggerTypes

external anyTypeToJson: 'value => JSON.t = "%identity"

let snakeCase = value =>
  value
  ->String.replaceRegExp(%re("/([A-Z]+)([A-Z][a-z])/g"), "$1_$2")
  ->String.replaceRegExp(%re("/([a-z0-9])([A-Z])/g"), "$1_$2")
  ->String.toLowerCase

let screamingSnakeCase = value =>
  value
  ->snakeCase
  ->String.replaceRegExp(%re("/[^a-zA-Z0-9]+/g"), "_")
  ->String.replaceRegExp(%re("/^_+|_+$/g"), "")
  ->String.toUpperCase

let variantConstructor = value => {
  let json = value->anyTypeToJson
  switch json->JSON.Decode.string {
  | Some(name) => name
  | None =>
    json
    ->JSON.Decode.object
    ->Option.flatMap(object => object->Dict.get("TAG"))
    ->Option.flatMap(JSON.Decode.string)
    ->Option.getOr("unknown")
  }
}

let variantName = value => value->variantConstructor->snakeCase

let spec = (event, ~action=Fact, ~outcome=?): eventSpec => {
  action,
  subject: event->variantName,
  outcome,
}

let eventName = (~category: category, ~action: action, ~subject, ~outcome: option<outcome>) => {
  let word = switch (action, outcome) {
  | (Fact, Some(outcome)) => (outcome :> string)
  | (Fact, None) => ""
  | (action, Some(outcome)) => `${(action :> string)}_${(outcome :> string)}`
  | (action, None) => (action :> string)
  }
  let prefix = (category :> string)->String.toLowerCase
  word === "" ? `${prefix}.${subject}` : `${prefix}.${word}.${subject}`
}
