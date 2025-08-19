open CommonUtils
open ConfigurationService
open SuperpositionTypes

@new external makeSet: unit => Js.t<'a> = "Set"
@send external add: (Js.t<'a>, string) => unit = "add"
@send external has: (Js.t<'a>, string) => bool = "has"

let defaultFieldConfig = {
  name: "",
  displayName: "",
  fieldType: TextInput,
  required: false,
  options: [],
  mergedFields: [],
  outputPath: "",
  defaultValue: "",
  component: Other,
}

let developmentContext = {
  eligibleConnectors: ["Stripe", "Adyen", "Cybersource", "Airwallex"],
  payment_method: "Card",
  payment_method_type: Some("Trustly"),
  country: Some("US"),
  mandate_type: Some("non_mandate"),
  collect_shipping_details_from_wallet_connector: Some("required"),
  collect_billing_details_from_wallet_connector: Some("required"),
}

let filterRequiredFields = fields => fields->Array.filter(field => field.required)

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

let parseResolvedConfigToFields = resolvedConfig => {
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
    let displayName = metadata->getString("display_name", baseName)
    let fieldType = metadata->getString("field_type", "")
    let required = metadata->getBool("required", false)
    let outputPath = metadata->getString("output_path", baseName)
    let defaultValue = metadata->getString("default_value", "")
    let options =
      metadata
      ->getOptionalArrayFromDict("options")
      ->Option.flatMap(arr =>
        arr
        ->Array.map(JSON.Decode.string)
        ->Array.filter(Option.isSome)
        ->Array.map(opt => opt->Option.getOr(""))
        ->Some
      )
      ->Option.getOr([])

    {
      name: baseName,
      displayName,
      fieldType: stringToFieldType(fieldType),
      required,
      options,
      outputPath,
      defaultValue,
      component: determineComponentFromField(baseName),
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

let sortFields = (fields, componentType) => {
  let getFieldPriority = (field: fieldConfig): int => {
    let fieldName = getFieldNameFromOutputPath(field.outputPath)
    switch componentType {
    | Card =>
      let cardField = stringToCardFieldName(fieldName)
      getCardFieldPriority(cardField)
    | Shipping
    | Billing =>
      let addressField = stringToAddressFieldName(fieldName)
      getAddressFieldPriority(addressField)
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
    collect_shipping_details_from_wallet_connector,
    collect_billing_details_from_wallet_connector,
  } = contextWithConnectorArray
  eligibleConnectors->Array.forEach(connector => {
    let resolvedConfig = configurationService->evaluateConfiguration({
      connector,
      payment_method,
      payment_method_type,
      country,
      mandate_type,
      collect_shipping_details_from_wallet_connector,
      collect_billing_details_from_wallet_connector,
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

let groupFieldsByComponentAndSortByPriority = fields => {
  let fieldsByComponent = Dict.make()

  fields->Array.forEach(field => {
    let component =
      field.component
      ->Option.getOr(Other)
      ->componentTypeToString
    let existingFields = fieldsByComponent->Dict.get(component)->Option.getOr([])
    fieldsByComponent->Dict.set(component, Array.concat(existingFields, [field]))
  })

  Some(
    componentsRenderPriorityEnum
    ->Array.map(componentType => {
      let componentName = componentTypeToString(componentType)
      let componentFields = fieldsByComponent->Dict.get(componentName)->Option.getOr([])
      (componentName, componentFields)
    })
    ->Array.filter(((_, fields)) => Array.length(fields) > 0),
  )
}

let removeDuplicateByOutputPath = fields => {
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
        combinedRequiredFields
        ->removeDuplicateByOutputPath
        ->groupFieldsByComponentAndSortByPriority
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
