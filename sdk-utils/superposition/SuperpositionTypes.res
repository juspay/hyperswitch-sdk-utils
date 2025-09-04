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
  | TextInput
  | PasswordInput
  | MonthSelect
  | YearSelect
  | DropdownSelect
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
  required: bool,
  options: array<string>,
  mergedFields: array<fieldConfig>,
  outputPath: string,
  defaultValue: string,
  component?: componentType,
}

type connectorArrayContext = {
  eligibleConnectors: array<string>,
  payment_method: string,
  payment_method_type: option<string>,
  country: option<string>,
  mandate_type: option<string>,
  collect_shipping_details_from_wallet_connector: option<string>,
  collect_billing_details_from_wallet_connector: option<string>,
}

type cardFieldName =
  | CardNumberNetworkMerged
  | CardNumber
  | CardNetwork
  | CardExpiryCvcMerged
  | CardExpMonth
  | CardExpYear
  | CardCvc
  | Other

type paymentFieldEnums =
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
  | _ => TextInput
  }
}

let stringToCardFieldName = str => {
  switch str {
  | "card_number_network_merged" => CardNumberNetworkMerged
  | "card_number" => CardNumber
  | "card_network" => CardNetwork
  | "card_expiry_cvc_merged" => CardExpiryCvcMerged
  | "card_exp_month" => CardExpMonth
  | "card_exp_year" => CardExpYear
  | "card_cvc" => CardCvc
  | _ => Other
  }
}

let stringToAddressFieldName = str => {
  switch str {
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

let fieldTypeToString = fieldType => {
  switch fieldType {
  | TextInput => "text_input"
  | PasswordInput => "password_input"
  | MonthSelect => "month_select"
  | YearSelect => "year_select"
  | DropdownSelect => "dropdown_select"
  | CountrySelect => "country_select"
  | EmailInput => "email_input"
  | PhoneInput => "phone_input"
  | CountryCodeSelect => "country_code_select"
  | DatePicker => "date_picker"
  | CurrencySelect => "currency_select"
  }
}

let cardFieldNameToString = cardFieldName => {
  switch cardFieldName {
  | CardNumberNetworkMerged => "card_number_network_merged"
  | CardNumber => "card_number"
  | CardNetwork => "card_network"
  | CardExpiryCvcMerged => "card_expiry_cvc_merged"
  | CardExpMonth => "card_exp_month"
  | CardExpYear => "card_exp_year"
  | CardCvc => "card_cvc"
  | _ => "other"
  }
}

let addressFieldNameToString = addressFieldName => {
  switch addressFieldName {
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
  | _ => "other"
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
  CardNumber,
  CardNetwork,
  CardExpiryCvcMerged,
  CardExpMonth,
  CardExpYear,
  CardCvc,
  Other,
]

let addressFieldsPriorityArray = [
  FullName,
  FirstName,
  LastName,
  Email,
  PhoneNumberWithCountryCode,
  CountryCode,
  Number,
  Line1,
  Line2,
  Line3,
  City,
  Zip,
  CityStateMerged,
  State,
  Country,
  Other,
]
