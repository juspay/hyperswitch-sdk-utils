open CommonUtils
open SuperpositionTypes

let convertConfigurationToFieldDefinitions = resolvedConfig => {
  let fieldGroups = Dict.make()

  resolvedConfig
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    let parts = key->String.split("._")
    switch (parts->Array.get(0), parts->Array.get(1)) {
    | (Some(baseName), Some(metadataKey)) if baseName !== "" && metadataKey !== "" => {
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
    | _ => ()
    }
  })

  fieldGroups
  ->Dict.toArray
  ->Array.map(((baseName, metadata)) => {
    {
      name: baseName,
      displayName: metadata->getString("display_name", baseName),
      fieldType: metadata->getString("field_type", "")->stringToFieldType,
      priority: metadata->getInt("priority", 1000),
      required: metadata->getBool("required", false),
      options: metadata
      ->getOptionalArrayFromDict("options")
      ->Option.map(arr => arr->Array.filterMap(JSON.Decode.string))
      ->Option.getOr([]),
      outputPath: metadata->getString("output_path", baseName),
      mergedFields: [],
    }
  })
}

let extractRequiredFieldsOnly = fields => fields->Array.filter(field => field.required)

let sortFieldsByPriorityOrder = fields => {
  fields->Array.sort((a, b) => Int.compare(a.priority, b.priority))
  fields
}

let removeDuplicateConnectors = fields => {
  let seen = Set.make()

  fields->Array.filter(f =>
    if Set.has(seen, f) {
      false
    } else {
      Set.add(seen, f)
      true
    }
  )
}

let removeShippingAndDuplicateFields = fields => {
  let seen = Set.make()

  fields->Array.filter(f =>
    if f.name->String.startsWith("shipping.") || Set.has(seen, f.outputPath) {
      false
    } else {
      Set.add(seen, f.outputPath)
      true
    }
  )
}

let extractFieldValuesFromPML = (required_fields: Dict.t<JSON.t>) => {
  let flatInitialValues = Dict.make()

  required_fields
  ->Dict.toArray
  ->Array.forEach(((_, fieldJson)) => {
    let requiredFieldDict = fieldJson->CommonUtils.getDictFromJson
    switch (requiredFieldDict->Dict.get("required_field"), requiredFieldDict->Dict.get("value")) {
    | (Some(fieldJson), Some(valueJson)) => {
        let field = CommonUtils.getStringFromJson(fieldJson, "")
        let value = CommonUtils.getStringFromJson(valueJson, "")
        if field !== "" {
          flatInitialValues->Dict.set(field, value)
        }
      }
    | _ => ()
    }
  })

  flatInitialValues
}

let filterFieldsBasedOnMissingData = (
  requiredFieldsFromSuperPosition: SuperpositionTypes.requiredFields,
  requiredFieldsFromPML,
) => {
  let firstNamePattern = "billing.address.first_name"
  let lastNamePattern = "billing.address.last_name"
  let phonePattern = "billing.phone"
  let addressPattern = "billing.address."

  let isFieldMissing = path => {
    switch requiredFieldsFromPML->Dict.get(path) {
    | Some("") | None => true
    | _ => false
    }
  }

  let (
    fieldCategories,
    nameFieldsMissing,
    phoneFieldsMissing,
    addressFieldsMissing,
  ) = requiredFieldsFromSuperPosition->Array.reduce(([], false, false, false), (
    (acc, nameMissing, phoneMissing, addressMissing),
    field,
  ) => {
    let path = field.outputPath
    let fieldMissing = isFieldMissing(path)

    let isNameField =
      path->String.includes(firstNamePattern) || path->String.includes(lastNamePattern)
    let isPhoneField = path->String.includes(phonePattern)
    let isAddressField = path->String.includes(addressPattern) && !isNameField

    let fieldCategory = (field, isNameField, isPhoneField, isAddressField)

    let newNameMissing = nameMissing || (isNameField && fieldMissing)
    let newPhoneMissing = phoneMissing || (isPhoneField && fieldMissing)
    let newAddressMissing = addressMissing || (isAddressField && fieldMissing)

    acc->Array.push(fieldCategory)
    (acc, newNameMissing, newPhoneMissing, newAddressMissing)
  })

  fieldCategories->Array.filterMap(((field, isNameField, isPhoneField, isAddressField)) => {
    let path = field.outputPath

    let shouldInclude =
      (isNameField && nameFieldsMissing) ||
      isPhoneField && phoneFieldsMissing ||
      isAddressField && addressFieldsMissing ||
      (!isNameField && !isPhoneField && !isAddressField && isFieldMissing(path))

    shouldInclude ? Some(field) : None
  })
}

let getOrCreateNestedDictionary = (dict: Dict.t<JSON.t>, key: string): Dict.t<JSON.t> => {
  switch dict->Dict.get(key) {
  | Some(json) => json->CommonUtils.getDictFromJson
  | None => Dict.make()
  }
}

let setValueAtNestedPath = (dict: Dict.t<JSON.t>, keys: array<string>, value: string): Dict.t<
  JSON.t,
> => {
  let keysLength = keys->Array.length

  if keysLength === 0 {
    dict
  } else if keysLength === 1 {
    let key = CommonUtils.getArrayElement(keys, 0, "")
    if key !== "" && value !== "" {
      dict->Dict.set(key, value->JSON.Encode.string)
    }
    dict
  } else {
    let currentDict = ref(dict)
    let pathLength = keysLength - 1

    for i in 0 to pathLength - 1 {
      let key = CommonUtils.getArrayElement(keys, i, "")
      if key !== "" {
        let nestedDict = getOrCreateNestedDictionary(currentDict.contents, key)
        currentDict.contents->Dict.set(key, nestedDict->JSON.Encode.object)
        currentDict := nestedDict
      }
    }

    let finalKey = CommonUtils.getArrayElement(keys, pathLength, "")
    if finalKey !== "" && value !== "" {
      currentDict.contents->Dict.set(finalKey, value->JSON.Encode.string)
    }

    dict
  }
}

let convertFlatDictToNestedObject = (flatDict: Dict.t<string>): Dict.t<JSON.t> => {
  let resultDict = Dict.make()
  flatDict
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    if key->String.length > 0 {
      setValueAtNestedPath(resultDict, key->String.split("."), value)->ignore
    }
  })
  resultDict
}

let convertConfigurationToRequiredFields = resolvedConfig => {
  let fieldGroups = Dict.make()
  resolvedConfig
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    let parts = key->String.split("._")
    switch (parts->Array.get(0), parts->Array.get(1)) {
    | (Some(baseName), Some(metadataKey)) if baseName !== "" && metadataKey !== "" => {
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
    | _ => ()
    }
  })

  fieldGroups
  ->Dict.toArray
  ->Array.filterMap(((baseName, metadata)) => {
    let required = metadata->getBool("required", false)
    if required {
      let displayName = metadata->getString("display_name", baseName)
      let fieldTypeStr = metadata->getString("field_type", "")
      let priority = metadata->getInt("priority", 1000)
      let outputPath = metadata->getString("output_path", baseName)
      let options =
        metadata
        ->getOptionalArrayFromDict("options")
        ->Option.map(arr => arr->Array.filterMap(JSON.Decode.string))
        ->Option.getOr([])

      Some({
        name: baseName,
        displayName,
        fieldType: fieldTypeStr->stringToFieldType,
        priority,
        required,
        options,
        outputPath,
        mergedFields: [],
      })
    } else {
      None
    }
  })
}
