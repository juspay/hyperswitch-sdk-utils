open CommonUtils

type criteriaRule = {
  criteria_value: string,
  eligible_connectors: array<string>,
}

type superpositionPaymentMethodType = {
  payment_method_type: string,
  payment_method_criteria: option<string>,
  criteria_rules: array<criteriaRule>,
  eligible_connectors: array<string>,
}

type superpositionPaymentMethod = {
  payment_method: string,
  payment_method_types: array<superpositionPaymentMethodType>,
}

type superpositionPaymentMethods = array<superpositionPaymentMethod>

let parseCriteriaRule = (json: JSON.t): criteriaRule => {
  let dict = json->getDictFromJson
  {
    criteria_value: dict->getString("criteria_value", ""),
    eligible_connectors: dict->getStrArray("eligible_connectors"),
  }
}

let parsePaymentMethodType = (json: JSON.t): superpositionPaymentMethodType => {
  let dict = json->getDictFromJson
  let criteriaRules = dict->getArray("criteria_rules")->Array.map(parseCriteriaRule)
  let eligibleConnectors = criteriaRules->Array.reduce([], (acc, rule) => {
    rule.eligible_connectors->Array.forEach(connector =>
      if !(acc->Array.includes(connector)) {
        acc->Array.push(connector)
      }
    )
    acc
  })
  {
    payment_method_type: dict->getString("payment_method_type", ""),
    payment_method_criteria: dict->getOptionString("payment_method_criteria"),
    criteria_rules: criteriaRules,
    eligible_connectors: eligibleConnectors,
  }
}

let parsePaymentMethod = (json: JSON.t): superpositionPaymentMethod => {
  let dict = json->getDictFromJson
  {
    payment_method: dict->getString("payment_method", ""),
    payment_method_types: dict->getArray("payment_method_types")->Array.map(parsePaymentMethodType),
  }
}

let parsePaymentMethods = (responseJson: JSON.t): option<superpositionPaymentMethods> =>
  responseJson
  ->JSON.Decode.object
  ->Option.flatMap(dict => dict->Dict.get("payment_methods"))
  ->Option.flatMap(JSON.Decode.array)
  ->Option.map(arr => arr->Array.map(parsePaymentMethod))
