// Maps a canonical (web `checkout_sdk.*`) config key to the mobile prop path,
// and — for the few keys whose value vocabulary differs — the token translation
// mobile needs. Canonical keys are the shared vocabulary both platforms author
// once in Superposition; web consumes them as-is, mobile translates them
// through the two tables below.

let mobileKeys = Map.fromArray([
  ("layout.type", "paymentMethodLayout.type"),
  (
    "layout.paymentMethodsArrangementForTabs",
    "paymentMethodLayout.paymentMethodsArrangementForTabs",
  ),
  ("layout.defaultCollapsed", "paymentMethodLayout.defaultCollapsed"),
  ("layout.radios", "paymentMethodLayout.radios"),
  ("layout.spacedAccordionItems", "paymentMethodLayout.spacedAccordionItems"),
  ("layout.maxAccordionItems", "paymentMethodLayout.maxAccordionItems"),
  ("layout.showCheckedIconForSelection", "paymentMethodLayout.showCheckedIconForSelection"),
  ("layout.cardBrandIcon", "paymentMethodLayout.cardBrandIcon"),
  ("layout.cvcIcon", "paymentMethodLayout.cvcIcon"),
  ("layout.displayOneClickPaymentMethodsOnTop", "paymentMethodLayout.showOneClickWalletsOnTop"),
  (
    "layout.savedMethodCustomization.hideCardExpiry",
    "paymentMethodLayout.savedMethodCustomization.hideCardExpiry",
  ),
  (
    "layout.savedMethodCustomization.defaultCollapsed",
    "paymentMethodLayout.savedMethodCustomization.defaultCollapsed",
  ),
  (
    "layout.savedMethodCustomization.hiddenPaymentMethods",
    "paymentMethodLayout.savedMethodCustomization.hiddenPaymentMethods",
  ),
  (
    "layout.savedMethodCustomization.groupingBehavior.displayInSeparateScreen",
    "paymentMethodLayout.savedMethodCustomization.groupingBehavior.displayInSeparateScreen",
  ),
  (
    "layout.savedMethodCustomization.groupingBehavior.groupByPaymentMethods",
    "paymentMethodLayout.savedMethodCustomization.groupingBehavior.groupByPaymentMethods",
  ),
  ("business.name", "merchantDisplayName"),
  ("paymentMethodsHeaderText", "paymentSheetHeaderLabel"),
  ("savedPaymentMethodsHeaderText", "savedPaymentSheetHeaderLabel"),
  ("subscriptionEvents", "subscribedEvents"),
  ("displaySavedPaymentMethods", "displaySavedPaymentMethods"),
  ("displaySavedPaymentMethodsCheckbox", "displaySavedPaymentMethodsCheckbox"),
  ("displayDefaultSavedPaymentIcon", "displayDefaultSavedPaymentIcon"),
  ("paymentMethodOrder", "paymentMethodOrder"),
  ("alwaysSendCustomerAcceptance", "alwaysSendCustomerAcceptance"),
  ("redirectionInfo", "redirectionInfo"),
  ("wallets.applePay", "walletButtonsConfiguration.applePay.visibility"),
  ("wallets.googlePay", "walletButtonsConfiguration.googlePay.visibility"),
  ("wallets.payPal", "walletButtonsConfiguration.payPal.visibility"),
  ("branding", "disableBranding"),
  (
    "layout.savedMethodCustomization.hideCVCError",
    "paymentMethodLayout.savedMethodCustomization.hideCVCError",
  ),
  (
    "layout.savedMethodCustomization.groupingBehavior.displayInSeparateSection",
    "paymentMethodLayout.savedMethodCustomization.groupingBehavior.displayInSeparateSection",
  ),
  (
    "layout.savedMethodCustomization.cvcIcon",
    "paymentMethodLayout.savedMethodCustomization.cvcIcon",
  ),
  ("splitCardFields", "splitCardFields"),
  ("preloadCardElement", "preloadCardElement"),
  ("opensCardScannerAutomatically", "opensCardScannerAutomatically"),
  ("stickyPayButton", "stickyPayButton"),
  ("primaryButtonLabel", "primaryButtonLabel"),
  ("allowsDelayedPaymentMethods", "allowsDelayedPaymentMethods"),
  ("allowsPaymentMethodsRequiringShippingAddress", "allowsPaymentMethodsRequiringShippingAddress"),
  ("locale", "locale"),
  (
    "walletButtonsConfiguration.googlePay.buttonType",
    "walletButtonsConfiguration.googlePay.buttonType",
  ),
  (
    "walletButtonsConfiguration.googlePay.buttonStyle.light",
    "walletButtonsConfiguration.googlePay.buttonStyle.light",
  ),
  (
    "walletButtonsConfiguration.googlePay.buttonStyle.dark",
    "walletButtonsConfiguration.googlePay.buttonStyle.dark",
  ),
  (
    "walletButtonsConfiguration.applePay.buttonType",
    "walletButtonsConfiguration.applePay.buttonType",
  ),
  (
    "walletButtonsConfiguration.applePay.buttonStyle.light",
    "walletButtonsConfiguration.applePay.buttonStyle.light",
  ),
  (
    "walletButtonsConfiguration.applePay.buttonStyle.dark",
    "walletButtonsConfiguration.applePay.buttonStyle.dark",
  ),
  ("walletButtonsConfiguration.payPal.buttonType", "walletButtonsConfiguration.payPal.buttonType"),
  ("walletButtonsConfiguration.payPal.buttonSize", "walletButtonsConfiguration.payPal.buttonSize"),
  (
    "walletButtonsConfiguration.payPal.buttonStyle.light",
    "walletButtonsConfiguration.payPal.buttonStyle.light",
  ),
  (
    "walletButtonsConfiguration.payPal.buttonStyle.dark",
    "walletButtonsConfiguration.payPal.buttonStyle.dark",
  ),
])

let shown = JSON.Encode.string("shown")
let hidden = JSON.Encode.string("hidden")
let autoNever = Map.fromArray([("auto", shown), ("never", hidden)])
let defaultHidden = Map.fromArray([("default", shown), ("hidden", hidden)])
let showHidden = Map.fromArray([("show", shown), ("hidden", hidden)])
let brandingToDisableBranding = Map.fromArray([
  ("auto", JSON.Encode.bool(false)),
  ("never", JSON.Encode.bool(true)),
])

let mobileValues = Map.fromArray([
  ("layout.cvcIcon", defaultHidden),
  ("layout.savedMethodCustomization.cvcIcon", defaultHidden),
  ("redirectionInfo", showHidden),
  ("branding", brandingToDisableBranding),
  ("wallets.applePay", autoNever),
  ("wallets.googlePay", autoNever),
  ("wallets.payPal", autoNever),
])

let forCanonicalPath = (canonicalPath, value) =>
  mobileKeys
  ->Map.get(canonicalPath)
  ->Option.flatMap(mobileKey =>
    switch mobileValues->Map.get(canonicalPath) {
    | None => Some((mobileKey, value))
    | Some(tokens) =>
      value
      ->JSON.Decode.string
      ->Option.flatMap(token => tokens->Map.get(token))
      ->Option.map(mobileValue => (mobileKey, mobileValue))
    }
  )
