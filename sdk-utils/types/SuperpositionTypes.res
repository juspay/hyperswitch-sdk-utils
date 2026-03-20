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
  | DropdownSelect
  // Payment-method-specific types
  | CryptoNetworkSelect // For CryptoCurrencyNetworks
  | VpaTextInput // For VpaId
  | PixKeyInput // For PixKey
  | PixCpfInput // For PixCPF
  | PixCnpjInput // For PixCNPJ
  | DocumentTypeSelect // For DocumentType
  | DocumentNumberInput // For DocumentNumber
  | BankAccountNumberInput // For BankAccountNumber
  | IbanInput // For IBAN
  | SourceBankAccountIdInput // For SourceBankAccountId
  | GiftCardNumberInput // For GiftCardNumber
  | GiftCardPinInput // For GiftCardPin
  | InfoElementType // For InfoElement
  // Address field types
  | AddressLine1Input
  | AddressLine2Input
  | AddressCityInput
  | AddressPostalCodeInput
  | AddressStateInput
  // | BillingNameInput
  | FullNameInput
  // | ShippingNameInput
  // | ShippingAddressLine1Input
  // | ShippingAddressLine2Input
  // | ShippingAddressCityInput
  // | ShippingAddressPostalCodeInput
  // | ShippingAddressStateInput
  // | ShippingAddressCountryInput
  | BankSelect
  | BankListSelect
  | BlikCodeInput
  // Additional types for complete PML compatibility
  | AddressCountryInput // For AddressCountry with options

type rec fieldConfig = {
  name: string,
  displayName: string,
  fieldType: fieldType,
  priority: int,
  required: bool,
  options: array<string>,
  outputPath: string,
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
  | "cvc_password_input" => CvcPasswordInput
  | "dropdown_select" => DropdownSelect
  // Payment-method-specific types
  | "crypto_network_select" => CryptoNetworkSelect
  | "vpa_text_input" => VpaTextInput
  | "pix_key_input" => PixKeyInput
  | "pix_cpf_input" => PixCpfInput
  | "pix_cnpj_input" => PixCnpjInput
  | "document_type_select" => DocumentTypeSelect
  | "document_number_input" => DocumentNumberInput
  | "bank_account_number_input" => BankAccountNumberInput
  | "iban_input" => IbanInput
  | "source_bank_account_id_input" => SourceBankAccountIdInput
  | "gift_card_number_input" => GiftCardNumberInput
  | "gift_card_pin_input" => GiftCardPinInput
  | "info_element_type" => InfoElementType
  | "bank_select" => BankSelect
  | "bank_list_select" => BankListSelect
  | "blik_code_input" => BlikCodeInput
  // Address types
  // | "address_line1_input" => AddressLine1Input
  // | "address_line2_input" => AddressLine2Input
  // | "address_city_input" => AddressCityInput
  // | "address_postal_code_input" => AddressPostalCodeInput
  // | "address_state_input" => AddressStateInput
  // | "billing_name_input" => BillingNameInput
  // | "full_name_input" => FullNameInput
  // | "shipping_name_input" => ShippingNameInput
  // | "shipping_address_line1_input" => ShippingAddressLine1Input
  // | "shipping_address_line2_input" => ShippingAddressLine2Input
  // | "shipping_address_city_input" => ShippingAddressCityInput
  // | "shipping_address_postal_code_input" => ShippingAddressPostalCodeInput
  // | "shipping_address_state_input" => ShippingAddressStateInput
  // | "shipping_address_country_input" => ShippingAddressCountryInput
  | _ => TextInput
  }
}
