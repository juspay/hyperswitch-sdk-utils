open CommonUtils
open SuperpositionTypes

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

let setValueAtNestedPathWithJson = (
  dict: Dict.t<JSON.t>,
  keys: array<string>,
  value: JSON.t,
): Dict.t<JSON.t> => {
  let keysLength = keys->Array.length

  if keysLength === 0 {
    dict
  } else if keysLength === 1 {
    let key = CommonUtils.getArrayElement(keys, 0, "")
    if key !== "" {
      dict->Dict.set(key, value)
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
    if finalKey !== "" {
      currentDict.contents->Dict.set(finalKey, value)
    }

    dict
  }
}

let convertFlatDictToNestedObjectWithJson = (flatDict: Dict.t<JSON.t>): Dict.t<JSON.t> => {
  let resultDict = Dict.make()
  flatDict
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    if key->String.length > 0 {
      setValueAtNestedPathWithJson(resultDict, key->String.split("."), value)->ignore
    }
  })
  resultDict
}

let rec removeEmptyObjects = (dict: Dict.t<JSON.t>): Dict.t<JSON.t> => {
  let cleanedDict = Dict.make()

  dict
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    switch value->JSON.Classify.classify {
    | Object(nestedDict) =>
      let cleaned = removeEmptyObjects(nestedDict)
      if cleaned->Dict.toArray->Array.length > 0 {
        cleanedDict->Dict.set(key, cleaned->JSON.Encode.object)
      }
    | _ => cleanedDict->Dict.set(key, value)
    }
  })

  cleanedDict
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
  resultDict->removeEmptyObjects
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
      })
    } else {
      None
    }
  })
}

let categorizedFields = fields => {
  fields->Array.reduce(([], [], [], [], [], [], [], []), (
    (
      cardFields,
      emailFields,
      billingNameFields,
      billingPhoneFields,
      billingOtherFields,
      cryptoFields,
      datePickerFields,
      otherFields,
    ),
    fieldConfig: SuperpositionTypes.fieldConfig,
  ) => {
    let fieldName = fieldConfig.name

    if fieldName->String.startsWith("card.") {
      cardFields->Array.push(fieldConfig)
    } else if fieldConfig.fieldType === EmailInput {
      emailFields->Array.push(fieldConfig)
    } else if (
      fieldName->String.includes("billing.address.first_name") ||
        fieldName->String.includes("billing.address.last_name")
    ) {
      billingNameFields->Array.push(fieldConfig)
    } else if fieldConfig.fieldType === CountryCodeSelect || fieldConfig.fieldType === PhoneInput {
      billingPhoneFields->Array.push(fieldConfig)
    } else if fieldName->String.includes("billing.address.") {
      billingOtherFields->Array.push(fieldConfig)
    } else if fieldName->String.includes("crypto.") {
      cryptoFields->Array.push(fieldConfig)
    } else if fieldConfig.fieldType === DatePicker {
      datePickerFields->Array.push(fieldConfig)
    } else {
      otherFields->Array.push(fieldConfig)
    }
    (
      cardFields,
      emailFields,
      billingNameFields,
      billingPhoneFields,
      billingOtherFields,
      cryptoFields,
      datePickerFields,
      otherFields,
    )
  })
}

// let createCustomField = (
//   outputPath,
//   ~fieldType=TextInput,
//   ~options=[],
//   ~displayName="",
//   ~name="",
// ) => {
//   name,
//   displayName,
//   fieldType,
//   priority: 99999,
//   required: true,
//   options,
//   outputPath,
// }
