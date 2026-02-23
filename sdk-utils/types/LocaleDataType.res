type localeTypes =
  | En
  | He
  | Fr
  | En_GB
  | Ar
  | Ja
  | De
  | Fr_BE
  | Es
  | Ca
  | Pt
  | It
  | Pl
  | Nl
  | NI_BE
  | Sv
  | Ru
  | Lt
  | Cs
  | Sk
  | Ls
  | Cy
  | El
  | Et
  | Fi
  | Nb
  | Bs
  | Da
  | Ms
  | Tr_CY

let localeTypeToString = locale => {
  switch locale {
  | Some(En) => "en"
  | Some(He) => "he"
  | Some(Fr) => "fr"
  | Some(En_GB) => "en-GB"
  | Some(Ar) => "ar"
  | Some(Ja) => "ja"
  | Some(De) => "de"
  | Some(Fr_BE) => "fr-BE"
  | Some(Es) => "es"
  | Some(Ca) => "ca"
  | Some(Pt) => "pt"
  | Some(It) => "it"
  | Some(Pl) => "pl"
  | Some(Nl) => "nl"
  | Some(NI_BE) => "nI-BE"
  | Some(Sv) => "sv"
  | Some(Ru) => "ru"
  | Some(Lt) => "lt"
  | Some(Cs) => "cs"
  | Some(Sk) => "sk"
  | Some(Ls) => "ls"
  | Some(Cy) => "cy"
  | Some(El) => "el"
  | Some(Et) => "et"
  | Some(Fi) => "fi"
  | Some(Nb) => "nb"
  | Some(Bs) => "bs"
  | Some(Da) => "da"
  | Some(Ms) => "ms"
  | Some(Tr_CY) => "tr-CY"
  | None => "en"
  }
}
let localeStringToType = locale => {
  // First try exact match (case-insensitive)
  let exactMatch = switch locale->String.toLowerCase {
  | "he" => Some(He)
  | "fr" => Some(Fr)
  | "en-gb" => Some(En_GB)
  | "ar" => Some(Ar)
  | "ja" => Some(Ja)
  | "de" => Some(De)
  | "fr-be" => Some(Fr_BE)
  | "es" => Some(Es)
  | "ca" => Some(Ca)
  | "pt" => Some(Pt)
  | "it" => Some(It)
  | "pl" => Some(Pl)
  | "nl" => Some(Nl)
  | "ni-be" => Some(NI_BE)
  | "sv" => Some(Sv)
  | "ru" => Some(Ru)
  | "lt" => Some(Lt)
  | "cs" => Some(Cs)
  | "sk" => Some(Sk)
  | "ls" => Some(Ls)
  | "cy" => Some(Cy)
  | "el" => Some(El)
  | "et" => Some(Et)
  | "fi" => Some(Fi)
  | "nb" => Some(Nb)
  | "bs" => Some(Bs)
  | "da" => Some(Da)
  | "ms" => Some(Ms)
  | "tr-cy" => Some(Tr_CY)
  | "en" => Some(En)
  | _ => None
  }

  // If exact match found, return it
  switch exactMatch {
  | Some(localeType) => Some(localeType)
  // If no exact match, try to match based on the first part (before "-")
  | None => {
      let baseLanguage =
        locale->String.toLowerCase->String.split("-")->Array.get(0)->Option.getOr("")
      switch baseLanguage {
      | "he" => Some(He)
      | "fr" => Some(Fr)
      | "ar" => Some(Ar)
      | "ja" => Some(Ja)
      | "de" => Some(De)
      | "es" => Some(Es)
      | "ca" => Some(Ca)
      | "pt" => Some(Pt)
      | "it" => Some(It)
      | "pl" => Some(Pl)
      | "nl" => Some(Nl)
      | "sv" => Some(Sv)
      | "ru" => Some(Ru)
      | "lt" => Some(Lt)
      | "cs" => Some(Cs)
      | "sk" => Some(Sk)
      | "ls" => Some(Ls)
      | "cy" => Some(Cy)
      | "el" => Some(El)
      | "et" => Some(Et)
      | "fi" => Some(Fi)
      | "nb" => Some(Nb)
      | "bs" => Some(Bs)
      | "da" => Some(Da)
      | "ms" => Some(Ms)
      | "tr" => Some(Tr_CY)
      | "en" => Some(En)
      | _ => Some(En) // Default fallback to English
      }
    }
  }
}

let localeStringToLocaleName = locale => {
  switch locale {
  | "BR" | "PT_BR" => "Portuguese (Brazil)"
  | "CN" | "ZH_CN" => "Chinese (Simplified)"
  | "DE" => "German"
  | "DK" | "DA" | "DA_DK" => "Danish"
  | "EN" => "English"
  | "ES" => "Spanish"
  | "FI" => "Finnish"
  | "FR" => "French"
  | "GR" | "EL" | "EL_GR" => "Greek"
  | "HR" => "Croatian"
  | "IT" => "Italian"
  | "JP" | "JA" | "JA_JP" => "Japanese"
  | "LA" | "ES_LA" => "Spanish (Latin America)"
  | "NL" => "Dutch"
  | "NO" => "Norwegian"
  | "PL" => "Polish"
  | "PT" => "Portuguese"
  | "RU" => "Russian"
  | "SV" | "SE" | "SV_SE" => "Swedish"
  | "ZH" | "TW" | "ZH_TW" => "Chinese (Traditional)"
  | x => x
  }
}

// NOTE: This type must match src/LocaleStrings/LocaleStringTypes.localeStrings
// All fields are plain strings for JSON compatibility
type localeStrings = {
  locale: string,
  localeDirection: string,
  cardNumberLabel: string,
  inValidCardErrorText: string,
  inCompleteCVCErrorText: string,
  inValidCVCErrorText: string,
  inCompleteExpiryErrorText: string,
  inValidExpiryErrorText: string,
  pastExpiryErrorText: string,
  poweredBy: string,
  validThruText: string,
  sortCodeText: string,
  cvcTextLabel: string,
  emailLabel: string,
  ibanEmptyText: string,
  ibanInvalidText: string,
  emailEmptyText: string,
  emailInvalidText: string,
  accountNumberText: string,
  accountNumberInvalidText: string,
  sortCodeInvalidText: string,
  fullNameLabel: string,
  line1Label: string,
  line1Placeholder: string,
  line1EmptyText: string,
  line2Label: string,
  line2Placeholder: string,
  line2EmptyText: string,
  cityLabel: string,
  cityEmptyText: string,
  postalCodeLabel: string,
  postalCodeEmptyText: string,
  postalCodeInvalidText: string,
  stateLabel: string,
  stateEmptyText: string,
  fullNamePlaceholder: string,
  countryLabel: string,
  currencyLabel: string,
  bankLabel: string,
  documentTypeLabel: string,
  redirectText: string,
  bankDetailsText: string,
  orPayUsing: string,
  addNewCard: string,
  useExisitingSavedCards: string,
  saveCardDetails: string,
  addBankAccount: string,
  achBankDebitTermsPart1: string,
  achBankDebitTermsPart2: string,
  sepaDebitTermsPart1: string,
  sepaDebitTermsPart2: string,
  sepaDebitTermsPart3: string,
  becsDebitTerms: string,
  cardTermsPart1: string,
  cardTermsPart2: string,
  payNowButton: string,
  cardNumberEmptyText: string,
  cardExpiryDateEmptyText: string,
  cvcNumberEmptyText: string,
  enterFieldsText: string,
  enterValidDetailsText: string,
  selectPaymentMethodText: string,
  card: string,
  surchargeMsgAmountPart1: string,
  surchargeMsgAmountPart2: string,
  surchargeMsgAmountForCardPart1: string,
  surchargeMsgAmountForCardPart2: string,
  surchargeMsgAmountForOneClickWallets: string,
  shortSurchargeMessagePart1: string,
  shortSurchargeMessagePart2: string,
  billingNameLabel: string,
  billingNamePlaceholder: string,
  cardHolderName: string,
  on: string,
  @as("and") and_: string,
  pleaseProvideText: string,
  // TODO: we can rename this variale to "pleaseProvideText"
  completeNameEmptyText: string,
  billingDetailsText: string,
  socialSecurityNumberLabel: string,
  saveWalletDetails: string,
  newPaymentMethods: string,
  useExistingPaymentMethods: string,
  cardNickname: string,
  nicknamePlaceholder: string,
  cardExpiredText: string,
  cardHeader: string,
  cardBrandConfiguredErrorText: string,
  blockedCardText: string,
  currencyNetwork: string,
  expiryPlaceholder: string,
  dateOfBirth: string,
  vpaIdLabel: string,
  vpaIdEmptyText: string,
  vpaIdInvalidText: string,
  dateofBirthRequiredText: string,
  dateOfBirthInvalidText: string,
  dateOfBirthPlaceholderText: string,
  formFundsInfoText: string,
  formFundsCreditInfoTextPart1: string,
  formFundsCreditInfoTextPart2: string,
  formEditText: string,
  formSaveText: string,
  formSubmitText: string,
  formSubmittingText: string,
  formSubheaderBillingDetailsText: string,
  formSubheaderCardText: string,
  formSubheaderAccountTextPart1: string,
  formSubheaderAccountTextPart2: string,
  formHeaderReviewText: string,
  formHeaderReviewTabLayoutTextPart1: string,
  formHeaderReviewTabLayoutTextPart2: string,
  formHeaderBankTextPart1: string,
  formHeaderBankTextPart2: string,
  formHeaderWalletTextPart1: string,
  formHeaderWalletTextPart2: string,
  formHeaderEnterCardText: string,
  formHeaderSelectBankText: string,
  formHeaderSelectWalletText: string,
  formHeaderSelectAccountText: string,
  formFieldACHRoutingNumberLabel: string,
  formFieldSepaIbanLabel: string,
  formFieldSepaBicLabel: string,
  formFieldPixIdLabel: string,
  formFieldBankAccountNumberLabel: string,
  formFieldPhoneNumberLabel: string,
  formFieldCountryCodeLabel: string,
  formFieldCountryCodeRequiredLabel: string,
  formFieldBankNameLabel: string,
  formFieldBankCityLabel: string,
  formFieldCardHoldernamePlaceholder: string,
  formFieldBankNamePlaceholder: string,
  formFieldBankCityPlaceholder: string,
  formFieldEmailPlaceholder: string,
  formFieldPhoneNumberPlaceholder: string,
  formFieldInvalidRoutingNumber: string,
  infoCardRefId: string,
  infoCardErrCode: string,
  infoCardErrMsg: string,
  infoCardErrReason: string,
  linkRedirectionTextPart1: string,
  linkRedirectionTextPart2: string,
  linkExpiryInfoPart1: string,
  linkExpiryInfoPart2: string,
  payoutFromTextPart1: string,
  payoutFromTextPart2: string,
  payoutStatusFailedMessage: string,
  payoutStatusPendingMessage: string,
  payoutStatusSuccessMessage: string,
  payoutStatusFailedText: string,
  payoutStatusPendingText: string,
  payoutStatusSuccessText: string,
  pixCNPJInvalidText: string,
  pixCNPJEmptyText: string,
  pixCNPJLabel: string,
  pixCNPJPlaceholder: string,
  pixCPFInvalidText: string,
  pixCPFEmptyText: string,
  pixCPFLabel: string,
  pixCPFPlaceholder: string,
  pixKeyEmptyText: string,
  pixKeyLabel: string,
  pixKeyPlaceholder: string,
  sourceBankAccountIdEmptyText: string,
  invalidCardHolderNameError: string,
  invalidNickNameError: string,
  invalidDigitsCardHolderNameError: string,
  expiry: string,
  payment_methods_afterpay_clearpay: string,
  payment_methods_google_pay: string,
  payment_methods_apple_pay: string,
  payment_methods_samsung_pay: string,
  payment_methods_mb_way: string,
  payment_methods_mobile_pay: string,
  payment_methods_ali_pay: string,
  payment_methods_ali_pay_hk: string,
  payment_methods_we_chat_pay: string,
  payment_methods_duit_now: string,
  payment_methods_revolut_pay: string,
  payment_methods_affirm: string,
  payment_methods_pay_safe_card: string,
  payment_methods_crypto_currency: string,
  payment_methods_card: string,
  payment_methods_klarna: string,
  payment_methods_sofort: string,
  payment_methods_ach_transfer: string,
  payment_methods_bacs_transfer: string,
  payment_methods_sepa_bank_transfer: string,
  payment_methods_instant_bank_transfer: string,
  payment_methods_instant_bank_transfer_finland: string,
  payment_methods_instant_bank_transfer_poland: string,
  payment_methods_sepa_debit: string,
  payment_methods_giropay: string,
  payment_methods_eps: string,
  payment_methods_walley: string,
  payment_methods_pay_bright: string,
  payment_methods_ach_debit: string,
  payment_methods_bacs_debit: string,
  payment_methods_becs_debit: string,
  payment_methods_blik: string,
  payment_methods_trustly: string,
  payment_methods_bancontact_card: string,
  payment_methods_online_banking_czech_republic: string,
  payment_methods_online_banking_slovakia: string,
  payment_methods_online_banking_finland: string,
  payment_methods_online_banking_poland: string,
  payment_methods_ideal: string,
  payment_methods_ban_connect: string,
  payment_methods_ach_bank_debit: string,
  payment_methods_przelewy24: string,
  payment_methods_interac: string,
  payment_methods_twint: string,
  payment_methods_vipps: string,
  payment_methods_dana: string,
  payment_methods_go_pay: string,
  payment_methods_kakao_pay: string,
  payment_methods_gcash: string,
  payment_methods_momo: string,
  payment_methods_touch_n_go: string,
  payment_methods_bizum: string,
  payment_methods_classic: string,
  payment_methods_online_banking_fpx: string,
  payment_methods_online_banking_thailand: string,
  payment_methods_alma: string,
  payment_methods_atome: string,
  payment_methods_multibanco_transfer: string,
  payment_methods_card_redirect: string,
  payment_methods_pay_by_bank: string,
  payment_methods_open_banking_pis: string,
  payment_methods_evoucher: string,
  payment_methods_pix_transfer: string,
  payment_methods_boleto: string,
  payment_methods_paypal: string,
  payment_methods_local_bank_transfer_transfer: string,
  payment_methods_mifinity: string,
  payment_methods_upi_collect: string,
  payment_methods_eft: string,
  payment_methods_givex: string,
  giftCardSectionTitle: string,
  giftCardNumberLabel: string,
  giftCardNumberPlaceholder: string,
  giftCardNumberEmptyText: string,
  giftCardNumberInvalidText: string,
  cardText: string,
  giftCardAppliedText: string,
  giftCardPinLabel: string,
  giftCardPinPlaceholder: string,
  giftCardPinEmptyText: string,
  giftCardPinInvalidText: string,
  giftCardPaymentCompleteMessage: string,
  giftCardPaymentRemainingMessagePart1: string,
  giftCardPaymentRemainingMessagePart2: string,
  cardDetailsLabel: string,
  enterValidCardNumberErrorText: string,
  firstName: string,
  lastName: string,
  requiredText: string,
  cardHolderNameRequiredText: string,
  lastNameRequiredText: string,
  nickNameLengthExceedError: string,
  cardExpiresText: string,
  addPaymentMethodLabel: string,
  walletDisclaimer: string,
  deletePaymentMethod: string,
  morePaymentMethods: string,
  useExisitingSavedCardsWeb: string,
  unsupportedCardErrorText: string,
  selectCardBrand: string,
  enterValidDigitsText: string,
  digitsText: string,
  enterValidIban: string,
  mandatoryFieldText: string,
  disclaimerTextAchTransfer: string,
  instructionalTextOfAchTransfer: string,
  accountDetailsText: string,
  achBankTransferText: string,
  bankName: string,
  swiftCode: string,
  doneText: string,
  copyToClipboard: string,
}

type constantStrings = {
  formFieldCardNumberPlaceholder: string,
  formFieldACHRoutingNumberPlaceholder: string,
  formFieldAccountNumberPlaceholder: string,
  formFieldSortCodePlaceholder: string,
  formFieldSepaIbanPlaceholder: string,
  formFieldSepaBicPlaceholder: string,
  formFieldPixIdPlaceholder: string,
  formFieldBankAccountNumberPlaceholder: string,
}

let defaultConstantStrings: constantStrings = {
  formFieldCardNumberPlaceholder: "****** 4242",
  formFieldACHRoutingNumberPlaceholder: "******",
  formFieldAccountNumberPlaceholder: "Account number",
  formFieldSortCodePlaceholder: "******",
  formFieldSepaIbanPlaceholder: "DE89 3704 0044 0532 0130 00",
  formFieldSepaBicPlaceholder: "AAAAUS33",
  formFieldPixIdPlaceholder: "Enter Pix key",
  formFieldBankAccountNumberPlaceholder: "Account number",
}

let defaultLocale: localeStrings = {
  locale: "en",
  localeDirection: "ltr",
  cardNumberLabel: "Card Number",
  inValidCardErrorText: "Card number is invalid.",
  inCompleteCVCErrorText: "Your card's security code is incomplete.",
  inValidCVCErrorText: "Your card's security code is invalid.",
  inCompleteExpiryErrorText: "Your card's expiration date is incomplete.",
  inValidExpiryErrorText: "Your card's expiration date is invalid.",
  pastExpiryErrorText: "Your card's expiration year is in the past.",
  poweredBy: "Powered By Hyperswitch",
  validThruText: "Expiry",
  sortCodeText: "Sort Code",
  cvcTextLabel: "CVC",
  emailLabel: "Email",
  ibanEmptyText: "IBAN cannot be empty",
  ibanInvalidText: "Please enter valid IBAN",
  emailEmptyText: "Email cannot be empty",
  emailInvalidText: "Invalid email address",
  accountNumberText: "Account Number",
  accountNumberInvalidText: "Account number is invalid",
  sortCodeInvalidText: "Sort code is invalid",
  fullNameLabel: "Full name",
  line1Label: "Address line 1",
  line1Placeholder: "Street address",
  line1EmptyText: "Address line 1 cannot be empty",
  line2Label: "Address line 2",
  line2Placeholder: "Apt., unit number, etc",
  line2EmptyText: "Address line 2 cannot be empty",
  cityLabel: "City",
  cityEmptyText: "City cannot be empty",
  postalCodeLabel: "Postal Code",
  postalCodeEmptyText: "Postal code cannot be empty",
  postalCodeInvalidText: "Invalid postal code",
  stateLabel: "State",
  stateEmptyText: "State cannot be empty",
  fullNamePlaceholder: "First and last name",
  countryLabel: "Country",
  currencyLabel: "Currency",
  bankLabel: "Select Bank",
  documentTypeLabel: "Doc Type",
  redirectText: "After submitting your order, you will be redirected to securely complete your purchase.",
  bankDetailsText: "After submitting these details, you will get bank account information to make payment. Please make sure to take a note of it.",
  orPayUsing: "Or pay using",
  addNewCard: "Add credit/debit card",
  useExisitingSavedCards: "Use saved payment methods",
  useExisitingSavedCardsWeb: "Use saved debit/credit cards.",
  saveCardDetails: "Save card details",
  addBankAccount: "Add bank account",
  achBankDebitTermsPart1: "Your ACH Debit Authorization will be set up now, but we'll confirm the amount and let you know before future payments are taken.",
  achBankDebitTermsPart2: "",
  sepaDebitTermsPart1: "By providing your payment information and confirming to this mandate form, you authorise (A) ",
  sepaDebitTermsPart2: ", the Creditor and/or our payment service provider(s) to send instructions to your bank to debit your account and (B) your bank to debit your account in accordance with the instructions from ",
  sepaDebitTermsPart3: ". As part of your rights, you are entitled to a refund from your bank under the terms and conditions of your agreement with your bank. A refund must be claimed within 8 weeks starting from the date on which your account was debited. Your rights are explained in a statement that you can obtain from your bank.",
  becsDebitTerms: "By providing your bank account details and confirming this payment, you agree to this Direct Debit Request and the Direct Debit Request service agreement and authorise Hyperswitch Payments Australia Pty Ltd ACN 160 180 343 Direct Debit User ID number 507156 (\"Hyperswitch\") to debit your account through the Bulk Electronic Clearing System (BECS) on behalf of Hyperswitch Payment Widget (the \"Merchant\") for any amounts separately communicated to you by the Merchant. You certify that you are either an account holder or an authorised signatory on the account listed above.",
  cardTermsPart1: "By providing your card information, you allow ",
  cardTermsPart2: " to charge your card for future payments in accordance with their terms.",
  payNowButton: "Pay Now",
  cardNumberEmptyText: "Card Number cannot be empty",
  cardExpiryDateEmptyText: "Card expiry date cannot be empty",
  cvcNumberEmptyText: "CVC Number cannot be empty",
  enterFieldsText: "Please enter all fields",
  enterValidDetailsText: "Please enter valid details",
  selectPaymentMethodText: "Please select a payment method and try again",
  card: "Card",
  surchargeMsgAmountPart1: "A surcharge amount of",
  surchargeMsgAmountPart2: "will be applied for this transaction",
  surchargeMsgAmountForCardPart1: "A surcharge amount of upto",
  surchargeMsgAmountForCardPart2: "will be applied for this transaction",
  surchargeMsgAmountForOneClickWallets: "Additional fee applicable",
  shortSurchargeMessagePart1: "Fee :",
  shortSurchargeMessagePart2: "",
  billingNameLabel: "Billing name",
  billingNamePlaceholder: "First and last name",
  cardHolderName: "Card Holder Name",
  on: "on",
  and_: "and",
  pleaseProvideText: "Please provide your ",
  completeNameEmptyText: "Please provide your complete ",
  // TODO: remove duplicate key "billingDetails" (for billingDetailsText) from all locales files.
  billingDetailsText: "Billing Details",
  socialSecurityNumberLabel: "Social Security Number",
  saveWalletDetails: "Wallets details will be saved upon selection",
  newPaymentMethods: "New payment methods",
  useExistingPaymentMethods: "Use saved payment methods",
  cardNickname: "Card Nickname",
  nicknamePlaceholder: "Card Nickname (Optional)",
  cardExpiredText: "This card has expired",
  cardHeader: "Card information",
  cardBrandConfiguredErrorText: " is not supported at the moment.",
  blockedCardText: "This card is not allowed for payments.",
  currencyNetwork: "Currency Networks",
  expiryPlaceholder: "MM / YY",
  dateOfBirth: "Date of Birth",
  vpaIdLabel: "Vpa Id",
  vpaIdEmptyText: "Vpa Id cannot be empty",
  vpaIdInvalidText: "Invalid Vpa Id address",
  dateofBirthRequiredText: "Date of birth is required",
  dateOfBirthInvalidText: "Age should be greater than or equal to 18 years",
  dateOfBirthPlaceholderText: "Enter Date of Birth",
  formFundsInfoText: "Funds will be credited to this account",
  formFundsCreditInfoTextPart1: "Your funds will be credited in the selected ",
  formFundsCreditInfoTextPart2: ".",
  formEditText: "Edit",
  formSaveText: "Save",
  formSubmitText: "Submit",
  formSubmittingText: "Submitting",
  formSubheaderBillingDetailsText: "Enter your billing address",
  formSubheaderCardText: "Your card details",
  formSubheaderAccountTextPart1: "Your account.",
  formSubheaderAccountTextPart2: "",
  formHeaderReviewText: "Review",
  formHeaderReviewTabLayoutTextPart1: "Review your ",
  formHeaderReviewTabLayoutTextPart2: " details.",
  formHeaderBankTextPart1: "Enter ",
  formHeaderBankTextPart2: " bank details.",
  formHeaderWalletTextPart1: "Enter ",
  formHeaderWalletTextPart2: " wallet details.",
  formHeaderEnterCardText: "Enter card details.",
  formHeaderSelectBankText: "Select a bank method.",
  formHeaderSelectWalletText: "Select a wallet.",
  formHeaderSelectAccountText: "Select an account for payouts.",
  formFieldACHRoutingNumberLabel: "Routing Number",
  formFieldSepaIbanLabel: "International Bank Account Number (IBAN)",
  formFieldSepaBicLabel: "Bank Identifier Code (Optional)",
  formFieldPixIdLabel: "Pix ID",
  formFieldBankAccountNumberLabel: "Bank Account Number",
  formFieldPhoneNumberLabel: "Phone Number",
  formFieldCountryCodeLabel: "Country Code (Optional)",
  formFieldCountryCodeRequiredLabel: "Country Code",
  formFieldBankNameLabel: "Bank Name (Optional)",
  formFieldBankCityLabel: "Bank City (Optional)",
  formFieldCardHoldernamePlaceholder: "Your Name",
  formFieldBankNamePlaceholder: "Bank Name",
  formFieldBankCityPlaceholder: "Bank City",
  formFieldEmailPlaceholder: "Your Email",
  formFieldPhoneNumberPlaceholder: "Your Phone",
  formFieldInvalidRoutingNumber: "Routing number is invalid.",
  infoCardRefId: "Ref Id",
  infoCardErrCode: "Error Code",
  infoCardErrMsg: "Error Message",
  infoCardErrReason: "Reason",
  linkRedirectionTextPart1: "Redirecting in ",
  linkRedirectionTextPart2: " seconds ...",
  linkExpiryInfoPart1: "Link expires on: ",
  linkExpiryInfoPart2: "",
  payoutFromTextPart1: "Payout from ",
  payoutFromTextPart2: "",
  payoutStatusFailedMessage: "Failed to process your payout. Please check with your provider for more details.",
  payoutStatusPendingMessage: "Your payout should be processed within 2-3 business days.",
  payoutStatusSuccessMessage: "Your payout was successful. Funds were deposited in your selected payment mode.",
  payoutStatusFailedText: "Payout Failed",
  payoutStatusPendingText: "Payout Processing",
  payoutStatusSuccessText: "Payout Successful",
  pixCNPJInvalidText: "Invalid Pix CNPJ.",
  pixCNPJEmptyText: "Pix CNPJ cannot be empty.",
  pixCNPJLabel: "Pix CNPJ",
  pixCNPJPlaceholder: "Enter Pix CNPJ.",
  pixCPFInvalidText: "Invalid Pix CPF.",
  pixCPFEmptyText: "Pix CPF cannot be empty.",
  pixCPFLabel: "Pix CPF",
  pixCPFPlaceholder: "Enter Pix CPF.",
  pixKeyEmptyText: "Pix key cannot be empty.",
  pixKeyPlaceholder: "Enter Pix key.",
  pixKeyLabel: "Pix key.",
  sourceBankAccountIdEmptyText: "Source Bank Account ID cannot be empty.",
  invalidCardHolderNameError: "Card Holder's name cannot have digits.",
  invalidNickNameError: "Nickname cannot have more than 2 digits.",
  invalidDigitsCardHolderNameError: "Card Holder's name cannot have digits.",
  expiry: "expiry",
  payment_methods_afterpay_clearpay: "After Pay",
  payment_methods_google_pay: "Google Pay",
  payment_methods_apple_pay: "Apple Pay",
  payment_methods_samsung_pay: "Samsung Pay",
  payment_methods_mb_way: "Mb Way",
  payment_methods_mobile_pay: "Mobile Pay",
  payment_methods_ali_pay: "Alipay",
  payment_methods_ali_pay_hk: "AlipayHK",
  payment_methods_we_chat_pay: "WeChat",
  payment_methods_duit_now: "DuitNow",
  payment_methods_revolut_pay: "Revolut Pay",
  payment_methods_affirm: "Affirm",
  payment_methods_pay_safe_card: "PaysafeCard",
  payment_methods_crypto_currency: "Crypto",
  payment_methods_card: "Card",
  payment_methods_klarna: "Klarna",
  payment_methods_sofort: "Sofort",
  payment_methods_ach_transfer: "ACH Bank Transfer",
  payment_methods_bacs_transfer: "BACS Bank Transfer",
  payment_methods_sepa_bank_transfer: "SEPA Bank Transfer",
  payment_methods_instant_bank_transfer: "Instant Bank Transfer",
  payment_methods_instant_bank_transfer_finland: "Instant Bank Transfer Finland",
  payment_methods_instant_bank_transfer_poland: "Instant Bank Transfer Poland",
  payment_methods_sepa_debit: "SEPA Debit",
  payment_methods_giropay: "GiroPay",
  payment_methods_eps: "EPS",
  payment_methods_walley: "Walley",
  payment_methods_pay_bright: "Pay Bright",
  payment_methods_ach_debit: "ACH Debit",
  payment_methods_bacs_debit: "BACS Debit",
  payment_methods_becs_debit: "BECS Debit",
  payment_methods_blik: "Blik",
  payment_methods_trustly: "Trustly",
  payment_methods_bancontact_card: "Bancontact Card",
  payment_methods_online_banking_czech_republic: "Online Banking CzechR",
  payment_methods_online_banking_slovakia: "Online Banking Slovakia",
  payment_methods_online_banking_finland: "Online Banking Finland",
  payment_methods_online_banking_poland: "Online Banking Poland",
  payment_methods_ideal: "iDEAL",
  payment_methods_ban_connect: "Ban Connect",
  payment_methods_ach_bank_debit: "ACH Direct Debit",
  payment_methods_przelewy24: "Przelewy24",
  payment_methods_interac: "Interac",
  payment_methods_twint: "Twint",
  payment_methods_vipps: "Vipps",
  payment_methods_dana: "Dana",
  payment_methods_go_pay: "Go Pay",
  payment_methods_kakao_pay: "Kakao Pay",
  payment_methods_gcash: "GCash",
  payment_methods_momo: "Momo",
  payment_methods_touch_n_go: "Touch N Go",
  payment_methods_bizum: "Bizum",
  payment_methods_classic: "Cash / Voucher",
  payment_methods_online_banking_fpx: "Online Banking Fpx",
  payment_methods_online_banking_thailand: "Online Banking Thailand",
  payment_methods_alma: "Alma",
  payment_methods_atome: "Atome",
  payment_methods_multibanco_transfer: "Multibanco",
  payment_methods_card_redirect: "Card",
  payment_methods_pay_by_bank: "Pay by Bank",
  payment_methods_open_banking_pis: "Open Banking",
  payment_methods_evoucher: "E-Voucher",
  payment_methods_pix_transfer: "Pix",
  payment_methods_boleto: "Boleto",
  payment_methods_paypal: "Paypal",
  payment_methods_local_bank_transfer_transfer: "Union Pay",
  payment_methods_mifinity: "Mifinity",
  payment_methods_upi_collect: "UPI Collect",
  payment_methods_eft: "EFT",
  payment_methods_givex: "Givex",
  giftCardSectionTitle: "Have a gift card?",
  giftCardNumberLabel: "Gift Card Number",
  giftCardNumberPlaceholder: "ABCD1234EFGH5678",
  giftCardNumberEmptyText: "Gift card number cannot be empty",
  giftCardNumberInvalidText: "Invalid gift card number",
  cardText: "Card",
  giftCardAppliedText: "applied",
  giftCardPinLabel: "Gift Card PIN",
  giftCardPinPlaceholder: "123456",
  giftCardPinEmptyText: "Gift card PIN cannot be empty",
  giftCardPinInvalidText: "Invalid gift card PIN",
  giftCardPaymentCompleteMessage: "No remaining amount to pay. Please proceed with payment.",
  giftCardPaymentRemainingMessagePart1: "Pay remaining ",
  giftCardPaymentRemainingMessagePart2: " with other payment method below.",
  cardDetailsLabel: "Card Details",
  enterValidCardNumberErrorText: "Please enter a valid card number.",
  firstName: "First name",
  lastName: "Last name",
  requiredText: "Required",
  cardHolderNameRequiredText: "Card Holder's name required",
  lastNameRequiredText: "Last Name Required",
  nickNameLengthExceedError: "Nickname cannot exceed 12 characters",
  cardExpiresText: "expires",
  addPaymentMethodLabel: "Add new payment method",
  walletDisclaimer: "Wallet details will be saved upon selection",
  deletePaymentMethod: "Delete",
  morePaymentMethods: "More payment methods",
  unsupportedCardErrorText: "Card brand is not supported.",
  selectCardBrand: "Select a card brand",
  enterValidDigitsText: "Please enter valid",
  digitsText: " digits",
  enterValidIban: "Please enter a valid Iban",
  mandatoryFieldText: "This field is mandatory",
  disclaimerTextAchTransfer: "Please make note of these details. You will not be able to generate this details again.",
  instructionalTextOfAchTransfer: "Use the below details to transfer amount",
  accountDetailsText: "Account details",
  achBankTransferText: "ACH Bank Transfer",
  bankName: "Bank name",
  swiftCode: "Swift code",
  doneText: "Done",
  copyToClipboard: "Copy to clipboard",
}
