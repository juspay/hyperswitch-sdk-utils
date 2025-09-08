open CommonUtils
open SuperpositionTypes

@new external makeSet: unit => Js.t<'a> = "Set"
@send external add: (Js.t<'a>, string) => unit = "add"
@send external has: (Js.t<'a>, string) => bool = "has"

type configurationService = {evaluateConfig: SuperpositionTypes.superpositionContext => Dict.t<JSON.t>}

let determineComponentFromField = (baseField: string): componentType => {
  switch baseField {
  | field if field->String.startsWith("card.") => Card
  | field if field->String.startsWith("billing.") => Billing
  | field if field->String.startsWith("shipping.") => Shipping
  | field
    if field->String.startsWith("bank_debit.") ||
    field->String.startsWith("bank_redirect.") ||
    field->String.startsWith("bank_transfer.") =>
    Bank
  | field if field->String.startsWith("wallet.") => Wallet
  | field if field->String.startsWith("crypto.") => Crypto
  | field if field->String.startsWith("upi.") => Upi
  | field if field->String.startsWith("voucher.") => Voucher
  | field if field->String.startsWith("gift_card.") => GiftCard
  | field if field->String.startsWith("mobile_payment.") => MobilePayment
  | field if field->String.startsWith("order_details.") => Other
  | "email" => Other
  | _ => Other
  }
}

let getFieldPriority = (~priorityArray, ~fieldName) => {
  let index = priorityArray->Array.findIndex(name => name === fieldName)
  index === -1 ? priorityArray->Array.length + 1 : index + 1
}

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
      component: determineComponentFromField(baseName),
      mergedFields: [],
    }
  })
}

let getFieldNameFromOutputPath = (outputPath, ~level=1) => {
  let parts = outputPath->String.split(".")
  if parts->Array.length > 0 {
    parts->Array.get(parts->Array.length - level)->Option.getOr("")
  } else {
    ""
  }
}

let defaultFieldConfig = {
  name: "",
  displayName: "",
  fieldType: TextInput,
  required: false,
  options: [],
  mergedFields: [],
  outputPath: "",
  component: Other,
}

let getParentPathFromOutputPath = outputPath =>
  outputPath->String.split(".")->Array.slice(~start=0, ~end=-1)->Array.join(".")

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
      getFieldPriority(~priorityArray=cardFieldsPriorityArray, ~fieldName=cardField)
    | Shipping
    | Billing =>
      let addressField = stringToAddressFieldName(fieldName)
      getFieldPriority(~priorityArray=addressFieldsPriorityArray, ~fieldName=addressField)
    | _ => 2
    }
  }

  fields->Array.toSorted((a, b) => {
    let priorityA = getFieldPriority(a)
    let priorityB = getFieldPriority(b)
    Int.toFloat(priorityA - priorityB)
  })
}

let filterRequiredFields = fields => fields->Array.filter(field => field.required)

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