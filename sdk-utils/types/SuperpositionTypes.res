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
  | FirstName
  | LastName
  | CryptoCurrency
  | CryptoNetwork
  | DateOfBirth
  | State
  | Country
  | LanguagePreference
  | BankNamesSelect

type rec fieldConfig = {
  intentDataReadPath: option<string>,
  defaultLabelText: string,
  defaultPlaceholderText: string,
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
  platform: string,
  country: string,
  mandate_type: string,
  collect_shipping_details_from_wallet_connector: string,
  collect_billing_details_from_wallet_connector: string,
  profile_id?: string,
  processor_merchant_id?: string,
  organization_id?: string,
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
  | "FirstName" => FirstName
  | "LastName" => LastName
  | "CryptoCurrency" => CryptoCurrency
  | "CryptoNetwork" => CryptoNetwork
  | "DateOfBirth" => DateOfBirth
  | "State" => State
  | "Country" => Country
  | "LanguagePreference" => LanguagePreference
  | "BankNamesSelect" => BankNamesSelect
  | _ => Generic
  }
}
