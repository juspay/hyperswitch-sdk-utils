type vaultingAction = Skip | Tokenize

type criteriaRule = {
  criteria_value: string,
  eligible_connectors: array<string>,
}

type sdkPaymentMethodType = {
  payment_method_type: string,
  payment_method_criteria: string,
  criteria_rules: array<criteriaRule>,
}

type sdkPaymentMethod = {
  payment_method: string,
  payment_method_types: array<sdkPaymentMethodType>,
}

type profile = {
  vaulting_action: vaultingAction,
  collect_shipping_details_from_wallet_connector: bool,
  collect_billing_details_from_wallet_connector: bool,
  always_collect_shipping_details_from_wallet_connector: bool,
  always_collect_billing_details_from_wallet_connector: bool,
}

type accountConfig = {profile: option<profile>}

type sdkConfigValue = {
  raw_configs: option<JSON.t>,
  payment_methods: array<sdkPaymentMethod>,
  account_config: option<accountConfig>,
}

let defaultSdkConfigValue: sdkConfigValue = {
  raw_configs: None,
  payment_methods: [],
  account_config: None,
}
