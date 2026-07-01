open CommonUtils
open SdkConfigTypes

let parseCriteriaRule = (json: JSON.t): criteriaRule => {
  let dict = json->getDictFromJson
  {
    criteria_value: dict->getString("criteria_value", ""),
    eligible_connectors: dict->getStrArray("eligible_connectors"),
  }
}

let parseSdkPaymentMethodType = (json: JSON.t): sdkPaymentMethodType => {
  let dict = json->getDictFromJson
  {
    payment_method_type: dict->getString("payment_method_type", ""),
    payment_method_criteria: dict->getString("payment_method_criteria", ""),
    criteria_rules: dict->getArray("criteria_rules")->Array.map(parseCriteriaRule),
  }
}

let parseSdkPaymentMethod = (json: JSON.t): sdkPaymentMethod => {
  let dict = json->getDictFromJson
  {
    payment_method: dict->getString("payment_method", ""),
    payment_method_types: dict
    ->getArray("payment_method_types")
    ->Array.map(parseSdkPaymentMethodType),
  }
}

let getVaultingActionFromName = (name: string): vaultingAction => {
  switch name {
  | "tokenize" => Tokenize
  | _ => Skip
  }
}

let parseProfile = (json: JSON.t): profile => {
  let dict = json->getDictFromJson
  {
    vaulting_action: dict
    ->getString("vaulting_action", "")
    ->getVaultingActionFromName,
    collect_shipping_details_from_wallet_connector: dict->getBool(
      "collect_shipping_details_from_wallet_connector",
      false,
    ),
    collect_billing_details_from_wallet_connector: dict->getBool(
      "collect_billing_details_from_wallet_connector",
      true,
    ),
    always_collect_shipping_details_from_wallet_connector: dict->getBool(
      "always_collect_shipping_details_from_wallet_connector",
      false,
    ),
    always_collect_billing_details_from_wallet_connector: dict->getBool(
      "always_collect_billing_details_from_wallet_connector",
      false,
    ),
  }
}

let parseProfileAccountConfig = (json: JSON.t): accountConfig => {
  let dict = json->getDictFromJson
  {
    profile: dict->Dict.get("profile")->Option.map(parseProfile),
  }
}

let parseContextUsed = (json: JSON.t): contextUsed => {
  let dict = json->getDictFromJson
  {
    profile_id: dict->getOptionString("profile_id"),
    processor_merchant_id: dict->getOptionString("processor_merchant_id"),
    organization_id: dict->getOptionString("organization_id"),
  }
}

let getCollectBillingDetailsFromWalletConnector = (profile: option<profile>): bool => {
  switch profile {
  | None => true
  | Some(p) =>
    if p.always_collect_billing_details_from_wallet_connector {
      true
    } else {
      p.collect_billing_details_from_wallet_connector
    }
  }
}

let getCollectShippingDetailsFromWalletConnector = (profile: option<profile>): bool => {
  switch profile {
  | None => false
  | Some(p) =>
    if p.always_collect_shipping_details_from_wallet_connector {
      true
    } else {
      p.collect_shipping_details_from_wallet_connector
    }
  }
}

let getProfileContext = (contextUsed: option<contextUsed>) => {
  switch contextUsed {
  | Some(context) => (context.profile_id, context.processor_merchant_id, context.organization_id)
  | None => (None, None, None)
  }
}

let itemToObjMapper = (json: JSON.t): sdkConfigValue => {
  let dict = json->getDictFromJson
  {
    raw_configs: dict->Dict.get("raw_configs"),
    payment_methods: dict->getArray("payment_methods")->Array.map(parseSdkPaymentMethod),
    account_config: dict
    ->Dict.get("account_config")
    ->Option.map(parseProfileAccountConfig),
    context_used: dict->Dict.get("context_used")->Option.map(parseContextUsed),
  }
}

let getEligibleConnectorsFromPaymentMethods = (
  paymentMethods: array<sdkPaymentMethod>,
  paymentMethod: string,
  paymentMethodType: string,
) => {
  let criteriaRules =
    paymentMethods
    ->Array.filter(pm => pm.payment_method === paymentMethod)
    ->Array.flatMap(pm => pm.payment_method_types)
    ->Array.filter(pmt => pmt.payment_method_type === paymentMethodType)
    ->Array.flatMap(pmt => pmt.criteria_rules)

  criteriaRules->Array.reduce([], (acc, rule) => {
    rule.eligible_connectors->Array.forEach(connector =>
      if !(acc->Array.includes(connector)) {
        acc->Array.push(connector)
      }
    )
    acc
  })
}
