type componentType =
  | Card
  | Billing
  | Shipping
  | Bank
  | Wallet
  | Crypto
  | Upi
  | Voucher
  | GiftCard
  | MobilePayment
  | Other

type fieldType =
  | CardNumberTextInput
  | CvcPasswordInput
  | TextInput
  | PasswordInput
  | MonthSelect
  | YearSelect
  | DropdownSelect
  | StateSelect
  | CountrySelect
  | EmailInput
  | PhoneInput
  | CountryCodeSelect
  | DatePicker
  | CurrencySelect

type context = {
  payment_method: string,
  payment_method_type: string,
  country: string,
  mandate_type: option<string>,
  collect_shipping_details_from_wallet_connector: bool,
  collect_billing_details_from_wallet_connector: bool,
}

type superpositionContext = {
  ...context,
  connector: string,
}

type connectorArrayContext = {
  ...context,
  eligibleConnectors: array<string>,
}

type paymentFieldEnums =
  | CardNumberNetworkMerged
  | CardNumber
  | CardNetwork
  | CardExpiryCvcMerged
  | CardExpMonth
  | CardExpYear
  | CardCvc
  | FullName
  | FirstName
  | LastName
  | Email
  | PhoneNumberWithCountryCode
  | CountryCode
  | Number
  | Line1
  | Line2
  | Line3
  | City
  | Zip
  | CityStateMerged
  | State
  | Country
  | AccountNumber
  | RoutingNumber
  | SortCode
  | BsbNumber
  | BecsSortCode
  | Iban
  | BlikCode
  | BankName
  | Issuer
  | Cnpj
  | Cpf
  | Key
  | SourceBankAccountId
  | DateOfBirth
  | LanguagePreference
  | Network
  | PayCurrency
  | VpaId
  | SocialSecurityNumber
  | Cvc
  | ClientUid
  | Msisdn
  | ProductName
  | ZipCountryMerged
  | Other

type rec fieldConfig = {
  name: string,
  displayName: string,
  fieldType: fieldType,
  fieldNameType: paymentFieldEnums,
  required: bool,
  options: array<string>,
  mergedFields: array<fieldConfig>,
  outputPath: string,
  component?: componentType,
  priority: int,
  shouldHide: bool,
}

type requiredFields = array<fieldConfig>

let stringToComponentType = str => {
  switch str {
  | "card" => Card
  | "billing" => Billing
  | "shipping" => Shipping
  | "bank" => Bank
  | "wallet" => Wallet
  | "crypto" => Crypto
  | "upi" => Upi
  | "voucher" => Voucher
  | "gift_card" => GiftCard
  | "mobile_payment" => MobilePayment
  | "other"
  | _ =>
    Other
  }
}

let stringToFieldType = str => {
  switch str {
  | "text_input" => TextInput
  | "password_input" => PasswordInput
  | "month_select" => MonthSelect
  | "year_select" => YearSelect
  | "dropdown_select" => DropdownSelect
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

let stringToFieldName = str => {
  switch str {
  | "card_number_network_merged" => CardNumberNetworkMerged
  | "card_number" => CardNumber
  | "card_network" => CardNetwork
  | "card_expiry_cvc_merged" => CardExpiryCvcMerged
  | "card_exp_month" => CardExpMonth
  | "card_exp_year" => CardExpYear
  | "card_cvc" => CardCvc
  | "full_name" => FullName
  | "first_name" => FirstName
  | "last_name" => LastName
  | "email" => Email
  | "phone_number_with_country_code" => PhoneNumberWithCountryCode
  | "country_code" => CountryCode
  | "number" => Number
  | "line1" => Line1
  | "line2" => Line2
  | "line3" => Line3
  | "city" => City
  | "zip" => Zip
  | "city_state_merged" => CityStateMerged
  | "state" => State
  | "country" => Country
  | "account_number" => AccountNumber
  | "routing_number" => RoutingNumber
  | "sort_code" => SortCode
  | "bsb_number" => BsbNumber
  | "becs_sort_code" => BecsSortCode
  | "iban" => Iban
  | "blik_code" => BlikCode
  | "bank_name" => BankName
  | "issuer" => Issuer
  | "cnpj" => Cnpj
  | "cpf" => Cpf
  | "key" => Key
  | "source_bank_account_id" => SourceBankAccountId
  | "date_of_birth" => DateOfBirth
  | "language_preference" => LanguagePreference
  | "network" => Network
  | "pay_currency" => PayCurrency
  | "vpa_id" => VpaId
  | "social_security_number" => SocialSecurityNumber
  | "cvc" => Cvc
  | "client_uid" => ClientUid
  | "msisdn" => Msisdn
  | "product_name" => ProductName
  | "zip_country_merged" => ZipCountryMerged
  | _ => Other
  }
}

let fieldNameToString = fieldName => {
  switch fieldName {
  | CardNumberNetworkMerged => "card_number_network_merged"
  | CardNumber => "card_number"
  | CardNetwork => "card_network"
  | CardExpiryCvcMerged => "card_expiry_cvc_merged"
  | CardExpMonth => "card_exp_month"
  | CardExpYear => "card_exp_year"
  | CardCvc => "card_cvc"
  | FullName => "full_name"
  | FirstName => "first_name"
  | LastName => "last_name"
  | Email => "email"
  | PhoneNumberWithCountryCode => "phone_number_with_country_code"
  | CountryCode => "country_code"
  | Number => "number"
  | Line1 => "line1"
  | Line2 => "line2"
  | Line3 => "line3"
  | City => "city"
  | Zip => "zip"
  | CityStateMerged => "city_state_merged"
  | State => "state"
  | Country => "country"
  | AccountNumber => "account_number"
  | RoutingNumber => "routing_number"
  | SortCode => "sort_code"
  | BsbNumber => "bsb_number"
  | BecsSortCode => "becs_sort_code"
  | Iban => "iban"
  | BlikCode => "blik_code"
  | BankName => "bank_name"
  | Issuer => "issuer"
  | Cnpj => "cnpj"
  | Cpf => "cpf"
  | Key => "key"
  | SourceBankAccountId => "source_bank_account_id"
  | DateOfBirth => "date_of_birth"
  | LanguagePreference => "language_preference"
  | Network => "network"
  | PayCurrency => "pay_currency"
  | VpaId => "vpa_id"
  | SocialSecurityNumber => "social_security_number"
  | Cvc => "cvc"
  | ClientUid => "client_uid"
  | Msisdn => "msisdn"
  | ProductName => "product_name"
  | ZipCountryMerged => "zip_country_merged"
  | Other => "other"
  }
}

let componentTypeToString = componentType => {
  switch componentType {
  | Card => "card"
  | Billing => "billing"
  | Shipping => "shipping"
  | Bank => "bank"
  | Wallet => "wallet"
  | Crypto => "crypto"
  | Upi => "upi"
  | Voucher => "voucher"
  | GiftCard => "gift_card"
  | MobilePayment => "mobile_payment"
  | Other => "other"
  }
}

let componentsRenderPriorityEnum = [
  Card,
  Billing,
  Shipping,
  Bank,
  Wallet,
  Crypto,
  Upi,
  Voucher,
  GiftCard,
  MobilePayment,
  Other,
]

let cardFieldsPriorityArray = [
  CardNumberNetworkMerged,
  // CardNumber,
  // CardNetwork,
  CardExpiryCvcMerged,
  // CardExpMonth,
  // CardExpYear,
  // CardCvc,
  Other,
]

let addressFieldsPriorityArray = [
  FullName,
  // FirstName,
  // LastName,
  // Email,
  PhoneNumberWithCountryCode,
  // CountryCode,
  // Number,
  // Line1,
  // Line2,
  // Line3,
  // City,
  // Zip,
  CityStateMerged,
  ZipCountryMerged,
  // State,
  // Country,
  Other,
]
