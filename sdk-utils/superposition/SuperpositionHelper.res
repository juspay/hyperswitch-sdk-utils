open CommonUtils
open ConfigurationService

// JS Set bindings
@new external makeSet: unit => Js.t<'a> = "Set"
@send external add: (Js.t<'a>, string) => unit = "add"
@send external has: (Js.t<'a>, string) => bool = "has"

type rec fieldConfig = {
  name: string,
  displayName: string,
  fieldType: string,
  required: bool,
  options: array<string>,
  mergedFields: array<fieldConfig>,
  outputPath: string,
  defaultValue: string,
  component?: string,
}

let defaultFieldConfig = {
  name: "",
  displayName: "",
  fieldType: "",
  required: false,
  options: [],
  mergedFields: [],
  outputPath: "",
  defaultValue: "",
  component: "",
}

type connectorArrayContext = {
  eligibleConnectors: array<string>,
  payment_method: string,
  payment_method_type: option<string>,
  country: option<string>,
  mandate_type: option<string>,
}

let developmentContext = {
  eligibleConnectors: ["Stripe", "Adyen", "Cybersource", "Airwallex"],
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

let cardFieldsPriority = [
  "card_number_network_merged",
  "card_number",
  "card_network",
  "card_expiry_cvc_merged",
  "card_exp_month",
  "card_exp_year",
  "card_cvc",
]

let addressFieldsPriority = [
  "full_name",
  "first_name",
  "last_name",
  "email",
  "phone_number_with_country_code",
  "country_code",
  "number",
  "line1",
  "line2",
  "line3",
  "city",
  "zip",
  "city_state_merged",
  "state",
  "country",
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
  | "email"
  | _ => "other"
  }
}

let getFieldNameFromOutputPath = (outputPath, ~level=1) => {
  let parts = outputPath->String.split(".")
  if parts->Array.length > 0 {
    parts->Array.get(parts->Array.length - level)->Option.getOr("")
  } else {
    ""
  }
}

let getParentPathFromOutputPath = outputPath =>
  outputPath->String.split(".")->Array.slice(~start=0, ~end=-1)->Array.join(".")

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
      mergedFields: [],
    }
  })
}

let mergeFields = (fields, fieldsToMerge, outputName, displayName, ~parent="") => {
  let foundFields =
    fieldsToMerge->Array.map(fieldName =>
      fields->Array.find(field => getFieldNameFromOutputPath(field.outputPath) === fieldName)
    )

  let allFieldsFound = foundFields->Array.every(field => field->Option.isSome)
  let hasSameParent = foundFields->Array.every(field =>
    switch field {
    | Some(f) => parent === "" || f.outputPath->getFieldNameFromOutputPath(~level=2) === parent
    | None => false
    }
  )

  if allFieldsFound && hasSameParent {
    let baseField = foundFields->Array.get(0)->Option.flatMap(x => x)->Option.getExn

    let mergedField = {
      ...baseField,
      name: baseField.name->getParentPathFromOutputPath ++ "." ++ outputName,
      displayName,
      outputPath: baseField.outputPath->getParentPathFromOutputPath ++ "." ++ outputName,
      mergedFields: foundFields->Array.map(f => f->Option.getOr(defaultFieldConfig)),
    }

    fields
    ->Array.filter(field => {
      let fieldName = getFieldNameFromOutputPath(field.outputPath)
      !(fieldsToMerge->Array.some(mergeFieldName => mergeFieldName === fieldName))
    })
    ->Array.concat([mergedField])
  } else {
    fields
  }
}

let getPriority = (~priorityArray, ~fieldName) => {
  let index = priorityArray->Array.findIndex(name => name === fieldName)
  index === -1 ? priorityArray->Array.length + 1 : index + 1
}

let sortFields = (fields, componentName) => {
  let getFieldPriority = (field: fieldConfig): int => {
    let fieldName = getFieldNameFromOutputPath(field.outputPath)
    switch componentName {
    | "card" => getPriority(~priorityArray=cardFieldsPriority, ~fieldName)
    | "shipping"
    | "billing" =>
      getPriority(~priorityArray=addressFieldsPriority, ~fieldName)
    | _ => 2
    }
  }

  fields->Array.toSorted((a, b) => {
    let priorityA = getFieldPriority(a)
    let priorityB = getFieldPriority(b)
    Belt.Int.toFloat(priorityA - priorityB)
  })
}

let getCombinedRequiredFieldsFromAllConnectors = contextWithConnectorArray => {
  let combinedRequiredFieldsFromAllConnectors = []
  let {
    eligibleConnectors,
    payment_method,
    payment_method_type,
    country,
    mandate_type,
  } = contextWithConnectorArray
  eligibleConnectors->Array.forEach(connector => {
    let resolvedConfig = configurationService->evaluateConfiguration({
      connector,
      payment_method,
      payment_method_type,
      country,
      mandate_type,
    })
    let fields = resolvedConfig->Option.map(parseResolvedConfigToFields)
    let requiredFields = fields->Option.map(filterRequiredFields)
    // let requiredFields = fields
    let _ =
      requiredFields->Option.map(fields =>
        fields->Array.forEach(field => combinedRequiredFieldsFromAllConnectors->Array.push(field))
      )
  })
  combinedRequiredFieldsFromAllConnectors
}

let groupFieldsByComponentAndSortByPriority = (fields: array<fieldConfig>) => {
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

let removeDuplicateByOutputPath = (fields: array<fieldConfig>): array<fieldConfig> => {
  let seen = makeSet()

  fields->Array.filter(f =>
    if has(seen, f.outputPath) {
      false
    } else {
      add(seen, f.outputPath)
      true
    }
  )
}

let initSuperpositionAndGetRequiredFields = async (~contextWithConnectorArray) => {
  try {
    let res = if configurationService->isInitialized {
      true
    } else {
      await configurationService->initialize
    }
    if res {
      // let combinedRequiredFields = developmentContext->getCombinedRequiredFieldsFromAllConnectors

      let combinedRequiredFields =
        contextWithConnectorArray->getCombinedRequiredFieldsFromAllConnectors

      if combinedRequiredFields->Array.length > 0 {
        let grouppedRequiredFields =
          combinedRequiredFields
          ->removeDuplicateByOutputPath
          ->groupFieldsByComponentAndSortByPriority
        grouppedRequiredFields
      } else {
        None
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
