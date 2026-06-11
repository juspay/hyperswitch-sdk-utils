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

type sdkConfigValue = {
  raw_configs: option<JSON.t>,
  payment_methods: array<sdkPaymentMethod>,
}

let defaultSdkConfigValue: sdkConfigValue = {
  raw_configs: None,
  payment_methods: [],
}
