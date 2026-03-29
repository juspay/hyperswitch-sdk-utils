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
  switch locale {
  | "he" => Some(He)
  | "fr" => Some(Fr)
  | "en-GB" => Some(En_GB)
  | "ar" => Some(Ar)
  | "ja" => Some(Ja)
  | "de" => Some(De)
  | "fr-BE" => Some(Fr_BE)
  | "es" => Some(Es)
  | "ca" => Some(Ca)
  | "pt" => Some(Pt)
  | "it" => Some(It)
  | "pl" => Some(Pl)
  | "nl" => Some(Nl)
  | "nI-BE" => Some(NI_BE)
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
  | "tr-CY" => Some(Tr_CY)
  | _ => Some(En)
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

type localeStrings = {
  locale: string,
  localeDirection: string,
  cardNumberLabel: string,
  cardDetailsLabel: string,
  inValidCardErrorText: string,
  inCompleteCVCErrorText: string,
  inValidCVCErrorText: string,
  inCompleteExpiryErrorText: string,
  inValidExpiryErrorText: string,
  pastExpiryErrorText: string,
  poweredBy: string,
  validThruText: string,
  sortCodeText: string,
  accountNumberText: string,
  cvcTextLabel: string,
  emailLabel: string,
  emailInvalidText: string,
  emailEmptyText: string,
  line1Label: string,
  line1Placeholder: string,
  line1EmptyText: string,
  line2Label: string,
  line2Placeholder: string,
  cityLabel: string,
  cityEmptyText: string,
  postalCodeLabel: string,
  postalCodeEmptyText: string,
  stateLabel: string,
  fullNameLabel: string,
  fullNamePlaceholder: string,
  countryLabel: string,
  currencyLabel: string,
  bankLabel: string,
  redirectText: string,
  bankDetailsText: string,
  orPayUsing: string,
  addNewCard: string,
  useExisitingSavedCards: string,
  saveCardDetails: string,
  addBankAccount: string,
  payNowButton: string,
  cardNumberEmptyText: string,
  cardExpiryDateEmptyText: string,
  cvcNumberEmptyText: string,
  enterFieldsText: string,
  enterValidDetailsText: string,
  card: string,
  billingNameLabel: string,
  cardHolderName: string,
  cardNickname: string,
  billingNamePlaceholder: string,
  firstName: string,
  lastName: string,
  billingDetails: string,
  requiredText: string,
  cardHolderNameRequiredText: string,
  invalidDigitsCardHolderNameError: string,
  lastNameRequiredText: string,
  nickNameLengthExceedError: string,
  invalidDigitsNickNameError: string,
  cardExpiresText: string,
  addPaymentMethodLabel: string,
  walletDisclaimer: string,
  deletePaymentMethod: string,
  enterValidCardNumberErrorText: string,
  line2EmptyText: string,
  postalCodeInvalidText: string,
  stateEmptyText: string,
  ibanEmptyText: string,
  selectPaymentMethodText: string,
  achBankDebitTermsPart1: string,
  achBankDebitTermsPart2: string,
  sepaDebitTermsPart1: string,
  sepaDebitTermsPart2: string,
  sepaDebitTermsPart3: string,
  becsDebitTerms: string,
  surchargeMsgAmountPart1: string,
  surchargeMsgAmountPart2: string,
  surchargeMsgAmountForCardPart1: string,
  surchargeMsgAmountForCardPart2: string,
  surchargeMsgAmountForOneClickWallets: string,
  shortSurchargeMessage: string,
  on: string,
  \"and": string,
  nameEmptyText: string,
  completeNameEmptyText: string,
  billingDetailsText: string,
  socialSecurityNumberLabel: string,
  saveWalletDetails: string,
  morePaymentMethods: string,
  useExistingPaymentMethods: string,
  nicknamePlaceholder: string,
  cardExpiredText: string,
  cardHeader: string,
  cardBrandConfiguredErrorText: string,
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
  pixKeyPlaceholder: string,
  pixKeyLabel: string,
  invalidCardHolderNameError: string,
  invalidNickNameError: string,
  cardTermsPart1: string,
  cardTermsPart2: string,
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

let defaultLocale = {
  locale: "en",
  localeDirection: "ltr",
  cardNumberLabel: "Card Number",
  cardDetailsLabel: "Card Details",
  inValidCardErrorText: "Card number is invalid.",
  inCompleteCVCErrorText: "Your card's security code is incomplete.",
  inValidCVCErrorText: "Your card's security code is invalid.",
  inCompleteExpiryErrorText: "Your card's expiration date is incomplete.",
  inValidExpiryErrorText: "Your card's expiration date is invalid.",
  pastExpiryErrorText: "Your card's expiration date is invalid",
  poweredBy: "Powered By Hyperswitch",
  validThruText: "Expiry",
  sortCodeText: "Sort Code",
  accountNumberText: "Account Number",
  cvcTextLabel: "CVC",
  emailLabel: "Email",
  emailInvalidText: "Invalid email address",
  emailEmptyText: "Email cannot be empty",
  line1Label: "Address line 1",
  line1Placeholder: "Street address",
  line1EmptyText: "Address line 1 cannot be empty",
  line2Label: "Address line 2",
  line2Placeholder: "Apt., unit number, etc (optional)",
  cityLabel: "City",
  cityEmptyText: "City cannot be empty",
  postalCodeLabel: "Postal Code",
  postalCodeEmptyText: "Postal code cannot be empty",
  stateLabel: "State",
  fullNameLabel: "Full name",
  fullNamePlaceholder: "First and last name",
  countryLabel: "Country",
  currencyLabel: "Currency",
  bankLabel: "Select Bank",
  redirectText: "After submitting your order, you will be redirected to securely complete your purchase.",
  bankDetailsText: "After submitting these details, you will get bank account information to make payment. Please make sure to take a note of it.",
  orPayUsing: "Or pay using",
  addNewCard: "Add credit/debit card",
  useExisitingSavedCards: "Use saved payment methods",
  saveCardDetails: "Save card details",
  addBankAccount: "Add bank account",
  payNowButton: "Pay Now",
  cardNumberEmptyText: "Card Number cannot be empty",
  cardExpiryDateEmptyText: "Card expiry date cannot be empty",
  cvcNumberEmptyText: "CVC Number cannot be empty",
  enterFieldsText: "Please enter all fields",
  enterValidDetailsText: "Please enter valid details",
  card: "Card",
  billingNameLabel: "Billing name",
  cardHolderName: "Card Holder Name",
  cardNickname: "Card Nickname",
  billingNamePlaceholder: "First and last name",
  firstName: "First name",
  lastName: "Last name",
  billingDetails: "Billing Details",
  requiredText: "Required",
  cardHolderNameRequiredText: "Card Holder's name required",
  invalidDigitsCardHolderNameError: "Card Holder's name cannot have digits",
  lastNameRequiredText: "Last Name Required",
  nickNameLengthExceedError: "Nickname cannot exceed 12 characters",
  invalidDigitsNickNameError: "Nickname cannot have more than 2 digits",
  cardExpiresText: "expires",
  addPaymentMethodLabel: "Add new payment method",
  walletDisclaimer: "Wallet details will be saved upon selection",
  deletePaymentMethod: "Delete",
  enterValidCardNumberErrorText: "Please enter a valid card number.",
  line2EmptyText: "Address line 2 cannot be empty.",
  postalCodeInvalidText: "Invalid postal code.",
  stateEmptyText: "State cannot be empty.",
  ibanEmptyText: "IBAN cannot be empty.",
  selectPaymentMethodText: "Please select a payment method and try again.",
  achBankDebitTermsPart1: "Your ACH Debit Authorization will be set up now, but we'll confirm the amount and let you know before future payments are taken.",
  achBankDebitTermsPart2: "",
  sepaDebitTermsPart1: "By providing your payment information and confirming to this mandate form, you authorise (A) ",
  sepaDebitTermsPart2: ", the Creditor and/or our payment service provider(s) to send instructions to your bank to debit your account and (B) your bank to debit your account in accordance with the instructions from ",
  sepaDebitTermsPart3: ". As part of your rights, you are entitled to a refund from your bank under the terms and conditions of your agreement with your bank. A refund must be claimed within 8 weeks starting from the date on which your account was debited. Your rights are explained in a statement that you can obtain from your bank.",
  becsDebitTerms: "By providing your bank account details and confirming this payment, you agree to this Direct Debit Request and the Direct Debit Request service agreement and authorise Hyperswitch Payments Australia Pty Ltd ACN 160 180 343 Direct Debit User ID number 507156 (\"Hyperswitch\") to debit your account through the Bulk Electronic Clearing System (BECS) on behalf of Hyperswitch Payment Widget (the \"Merchant\") for any amounts separately communicated to you by the Merchant. You certify that you are either an account holder or an authorised signatory on the account listed above.",
  surchargeMsgAmountPart1: "A surcharge amount of ",
  surchargeMsgAmountPart2: " will be applied for this transaction.",
  surchargeMsgAmountForCardPart1: "A surcharge amount of up to ",
  surchargeMsgAmountForCardPart2: " will be applied for this transaction.",
  surchargeMsgAmountForOneClickWallets: "Additional fee applicable.",
  shortSurchargeMessage: "Fee : ",
  on: "on",
  \"and": "and",
  nameEmptyText: "Please provide your name.",
  completeNameEmptyText: "Please provide your complete name.",
  billingDetailsText: "Billing Details",
  socialSecurityNumberLabel: "Social Security Number",
  saveWalletDetails: "Wallet details will be saved upon selection.",
  morePaymentMethods: "More payment methods",
  useExistingPaymentMethods: "Use saved payment methods",
  nicknamePlaceholder: "Card Nickname (Optional)",
  cardExpiredText: "This card has expired.",
  cardHeader: "Card information",
  cardBrandConfiguredErrorText: " is not supported at the moment.",
  currencyNetwork: "Currency Networks",
  expiryPlaceholder: "MM / YY",
  dateOfBirth: "Date of Birth",
  vpaIdLabel: "Vpa Id",
  vpaIdEmptyText: "Vpa Id cannot be empty.",
  vpaIdInvalidText: "Invalid Vpa Id address.",
  dateofBirthRequiredText: "Date of birth is required.",
  dateOfBirthInvalidText: "Age should be greater than or equal to 18 years.",
  dateOfBirthPlaceholderText: "Enter Date of Birth.",
  formFundsInfoText: "Funds will be credited to this account.",
  formFundsCreditInfoTextPart1: "Your funds will be credited in the selected ",
  formFundsCreditInfoTextPart2: ".",
  formEditText: "Edit",
  formSaveText: "Save",
  formSubmitText: "Submit",
  formSubmittingText: "Submitting",
  formSubheaderBillingDetailsText: "Enter your billing address.",
  formSubheaderCardText: "Your card details.",
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
  invalidCardHolderNameError: "Card Holder's name cannot have digits.",
  invalidNickNameError: "Nickname cannot have more than 2 digits.",
  cardTermsPart1: "By providing your card information, you allow ",
  cardTermsPart2: " to charge your card for future payments in accordance with their terms.",
  useExisitingSavedCardsWeb: "Use saved debit/credit cards.",
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
