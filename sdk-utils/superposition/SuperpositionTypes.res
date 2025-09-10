type fieldType =
  | CardNumberTextInput
  | CvcPasswordInput
  | TextInput
  | PasswordInput
  | MonthSelect
  | YearSelect
  | StateSelect
  | CountrySelect
  | EmailInput
  | PhoneInput
  | CountryCodeSelect
  | DatePicker
  | CurrencySelect

type rec fieldConfig = {
  name: string,
  displayName: string,
  fieldType: fieldType,
  priority: int,
  required: bool,
  options: array<string>,
  mergedFields: array<fieldConfig>,
  outputPath: string,
}

type requiredFields = array<fieldConfig>

type superpositionBaseContext = {
  payment_method: string,
  payment_method_type: string,
  country: string,
  mandate_type: option<string>,
  collect_shipping_details_from_wallet_connector: bool,
  collect_billing_details_from_wallet_connector: bool,
}

type superpositionContext = {
  ...superpositionBaseContext,
  connector: string,
}

let stringToFieldType = str => {
  switch str {
  | "text_input" => TextInput
  | "password_input" => PasswordInput
  | "month_select" => MonthSelect
  | "year_select" => YearSelect
  | "country_select" => CountrySelect
  | "email_input" => EmailInput
  | "phone_input" => PhoneInput
  | "country_code_select" => CountryCodeSelect
  | "date_picker" => DatePicker
  | "currency_select" => CurrencySelect
  | "state_select" => StateSelect
  | "card_number_text_input" => CardNumberTextInput
  | "cvc_pasword_input" => CvcPasswordInput
  | _ => TextInput
  }
}