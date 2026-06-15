type fieldType =
  | Generic
  | Dropdown
  | Phone
  | Date
  | CardNumber
  | Cvc
  | Email
  | CardHolderName
  | CardExpiryMonth
  | CardExpiryYear
  | CardNetwork
  | PhoneCountryCode
  | CryptoCurrency
  | CryptoNetwork
  | DateOfBirth
  | State
  | Country

type rec fieldConfig = {
  intentDataReadPath: option<string>,
  defaultLabelText: string,
  fieldRenderType: fieldType,
  fieldDisplayOrder: int,
  isRequired: bool,
  dropdownOptions: option<array<string>>,
  confirmRequestWritePath: string,
  renderWhenPrefilled: option<bool>,
  validationRuleType: option<string>,
  validationRegexPattern: option<string>,
  labelLocalizationKey: option<string>,
  placeholderLocalizationKey: option<string>,
  inputFormatPattern: option<string>,
  htmlAutocompleteAttribute: option<string>,
  keyboardType: option<string>,
  maxInputLength: option<int>,
  layoutRowId: option<string>,
  layoutWidthRatio: option<float>,
  merchantProvidedDisplayName: option<string>,
  merchantProvidedPlaceholderText: option<string>,
}

type requiredFields = array<fieldConfig>

type superpositionBaseContext = {
  payment_method: string,
  payment_method_type: string,
  country: string,
  mandate_type: string,
  collect_shipping_details_from_wallet_connector: string,
  collect_billing_details_from_wallet_connector: string,
}

type superpositionContext = {
  ...superpositionBaseContext,
  connector: string,
}

let stringToFieldType = str => {
  switch str {
  | "Dropdown" => Dropdown
  | "Phone" => Phone
  | "Date" => Date
  | "CardNumber" => CardNumber
  | "Cvc" => Cvc
  | "Email" => Email
  | "CardHolderName" => CardHolderName
  | "CardExpiryMonth" => CardExpiryMonth
  | "CardExpiryYear" => CardExpiryYear
  | "CardNetwork" => CardNetwork
  | "PhoneCountryCode" => PhoneCountryCode
  | "CryptoCurrency" => CryptoCurrency
  | "CryptoNetwork" => CryptoNetwork
  | "DateOfBirth" => DateOfBirth
  | "State" => State
  | "Country" => Country
  | _ => Generic
  }
}
