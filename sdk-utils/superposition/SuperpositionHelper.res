open CommonUtils
open ConfigurationService
type fieldConfig = {
  name: string,
  displayName: string,
  fieldType: string,
  required: bool,
  options: array<string>,
  outputPath: string,
  defaultValue: string,
  component?: string,
}

let developmentContext: ConfigurationService.context = {
  connector: "Airwallex",
  payment_method: "Card",
  payment_method_type: Some("Trustly"),
  country: Some("US"),
  mandate_type: Some("non_mandate"),
}

let componentsRenderPriority = [
  "card",
  "billing",
  "shipping",
  "bank",
  "wallet",
  "crypto",
  "upi",
  "voucher",
  "gift_card",
  "mobile_payment",
  "other",
]

let configInputTypes = [
  "text_input",
  "password_input",
  "month_select",
  "year_select",
  "dropdown_select",
  "country_select",
  "email_input",
  "phone_input",
  "country_code_select",
  "date_picker",
  "currency_select",
]

let filterRequiredFields = (fields: array<fieldConfig>) => {
  fields->Array.filter(field => field.required)
}

let determineComponent = (baseField: string): string => {
  switch baseField {
  | field if field->String.startsWith("card.") => "card"
  | field if field->String.startsWith("billing.") => "billing"
  | field if field->String.startsWith("shipping.") => "shipping"
  | field
    if field->String.startsWith("bank_debit.") ||
    field->String.startsWith("bank_redirect.") ||
    field->String.startsWith("bank_transfer.") => "bank"
  | field if field->String.startsWith("wallet.") => "wallet"
  | field if field->String.startsWith("crypto.") => "crypto"
  | field if field->String.startsWith("upi.") => "upi"
  | field if field->String.startsWith("voucher.") => "voucher"
  | field if field->String.startsWith("gift_card.") => "gift_card"
  | field if field->String.startsWith("mobile_payment.") => "mobile_payment"
  | field if field->String.startsWith("order_details.") => "other"
  | "email" => "billing"
  | _ => "other"
  }
}

let extractFieldFromConfig = (key: string, value: JSON.t): option<fieldConfig> => {
  switch value->JSON.Classify.classify {
  | Object(dict) => {
      let name = key
      let displayName = dict->getString("display_name", key)
      let fieldType = dict->getString("field_type", "")
      let required = dict->getBool("required", false)
      let options = dict->getStrArray("options")
      let outputPath = dict->getString("output_path", key)
      let defaultValue = dict->getString("default_value", "")

      Some({
        name,
        displayName,
        fieldType,
        required,
        options,
        outputPath,
        defaultValue,
      })
    }
  | _ => None
  }
}

let getFieldNameFromOutputPath = (outputPath: string): string => {
  let parts = outputPath->String.split(".")
  if parts->Array.length > 0 {
    parts->Array.get(parts->Array.length - 1)->Option.getOr("")
  } else {
    ""
  }
}

let parseResolvedConfigToFields = (resolvedConfig: ConfigurationService.resolvedConfig): array<
  fieldConfig,
> => {
  let fieldGroups = Dict.make()

  resolvedConfig
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    let parts = key->String.split("._")
    if parts->Array.length >= 2 {
      let baseName = parts->Array.get(0)->Option.getOr("")
      let metadataKey = parts->Array.get(1)->Option.getOr("")

      let fieldGroup = switch fieldGroups->Dict.get(baseName) {
      | Some(group) => group
      | None => {
          let newGroup = Dict.make()
          fieldGroups->Dict.set(baseName, newGroup)
          newGroup
        }
      }

      fieldGroup->Dict.set(metadataKey, value)
    }
  })

  fieldGroups
  ->Dict.toArray
  ->Array.map(((baseName, metadata)) => {
    let displayName =
      metadata->Dict.get("display_name")->Option.flatMap(JSON.Decode.string)->Option.getOr(baseName)
    let fieldType =
      metadata->Dict.get("field_type")->Option.flatMap(JSON.Decode.string)->Option.getOr("")
    let required =
      metadata->Dict.get("required")->Option.flatMap(JSON.Decode.bool)->Option.getOr(false)
    let outputPath =
      metadata->Dict.get("output_path")->Option.flatMap(JSON.Decode.string)->Option.getOr(baseName)
    let defaultValue =
      metadata->Dict.get("default_value")->Option.flatMap(JSON.Decode.string)->Option.getOr("")
    let options =
      metadata
      ->Dict.get("options")
      ->Option.flatMap(JSON.Decode.array)
      ->Option.flatMap(arr =>
        arr
        ->Array.map(JSON.Decode.string)
        ->Array.filter(Option.isSome)
        ->Array.map(opt => opt->Option.getExn)
        ->Some
      )
      ->Option.getOr([])

    {
      name: baseName,
      displayName,
      fieldType,
      required,
      options,
      outputPath,
      defaultValue,
      component: determineComponent(baseName),
    }
  })
}

let initSuperpositionAndGetRequiredFields = async () => {
  try {
    let res = await configurationService->initialize
    if res {
      let resolvedConfig = configurationService->evaluateConfiguration(developmentContext)
      let fields = resolvedConfig->Option.map(parseResolvedConfigToFields)
      let requiredFields = fields->Option.map(filterRequiredFields)
      //   let requiredFields = fields
      switch requiredFields {
      | Some(fields) => {
          let fieldsByComponent = Dict.make()

          fields->Array.forEach(field => {
            let component = field.component->Option.getOr("other")
            let existingFields = fieldsByComponent->Dict.get(component)->Option.getOr([])
            fieldsByComponent->Dict.set(component, Array.concat(existingFields, [field]))
          })

          Some(
            componentsRenderPriority
            ->Array.map(componentName => {
              let componentFields = fieldsByComponent->Dict.get(componentName)->Option.getOr([])
              (componentName, componentFields)
            })
            ->Array.filter(((_, fields)) => Array.length(fields) > 0),
          )
        }
      | None => None
      }
    } else {
      Console.error("Configuration not initialized, cannot get required fields")
      None
    }
  } catch {
  | err =>
    Console.error2("Error evaluating configuration:", err)
    None
  }
}
