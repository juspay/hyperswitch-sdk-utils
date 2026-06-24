open CommonUtils
open SuperpositionTypes

let findFieldByName = (fields: array<fieldConfig>, name) => fields->Array.find(f => f.name === name)

let sortFieldsByPriorityOrder = fields => {
  fields->Array.sort((a, b) => Int.compare(a.fieldDisplayOrder, b.fieldDisplayOrder))
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
    if (
      f.intentDataReadPath->Option.mapOr(false, s => s->String.startsWith("shipping.")) ||
        Set.has(seen, f.confirmRequestWritePath)
    ) {
      false
    } else {
      Set.add(seen, f.confirmRequestWritePath)
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

let resolveFieldValue = (field, intentDataDict) =>
  switch field.intentDataReadPath {
  | Some(readPath) =>
    switch getStringAtPath(intentDataDict, readPath) {
    | Some(val) if val !== "" => Some(val)
    | _ => None
    }
  | None => None
  }

let filterFieldsBasedOnMissingData = (
  requiredFieldsFromSuperPosition: SuperpositionTypes.requiredFields,
  intentDataDict,
) => {
  let firstNamePattern = "billing.address.first_name"
  let lastNamePattern = "billing.address.last_name"
  let phonePattern = "billing.phone"
  let addressPattern = "billing.address."

  let isFieldMissing = field => resolveFieldValue(field, intentDataDict)->Option.isNone

  let (
    fieldCategories,
    nameFieldsMissing,
    phoneFieldsMissing,
    addressFieldsMissing,
  ) = requiredFieldsFromSuperPosition->Array.reduce(([], false, false, false), (
    (acc, nameMissing, phoneMissing, addressMissing),
    field,
  ) => {
    let path = field.confirmRequestWritePath
    let fieldMissing = isFieldMissing(field)

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
    let shouldInclude =
      (isNameField && nameFieldsMissing) ||
      isPhoneField && phoneFieldsMissing ||
      isAddressField && addressFieldsMissing ||
      (!isNameField && !isPhoneField && !isAddressField && isFieldMissing(field))

    shouldInclude ? Some(field) : None
  })
}

let buildInitialValuesFromIntentData = (
  fields: SuperpositionTypes.requiredFields,
  intentDataDict: Dict.t<JSON.t>,
): Dict.t<string> => {
  fields->Array.reduce(Dict.make(), (acc, field) => {
    switch resolveFieldValue(field, intentDataDict) {
    | Some(val) => acc->Dict.set(field.confirmRequestWritePath, val)
    | None => ()
    }
    acc
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
    let isRequired = metadata->getBool("is_required", false)
    if isRequired {
      let confirmRequestWritePath = metadata->getString("confirm_request_write_path", baseName)
      let defaultLabelText = metadata->getString("default_label_text", baseName)
      let defaultPlaceholderText = metadata->getString("default_placeholder_text", "")
      let fieldRenderTypeStr = metadata->getString("field_render_type", "")
      let fieldDisplayOrder = metadata->getInt("field_display_order", 1000)
      let dropdownOptions =
        metadata
        ->getOptionalArrayFromDict("dropdown_options")
        ->Option.map(arr => arr->Array.filterMap(JSON.Decode.string))
      let intentDataReadPath = metadata->getOptionString("intent_data_read_path")
      let renderWhenPrefilled = metadata->getOptionBool("render_when_prefilled")
      let validationRuleType = metadata->getOptionString("validation_rule_type")
      let validationRegexPattern = metadata->getOptionString("validation_regex_pattern")
      let labelLocalizationKey = metadata->getOptionString("label_localization_key")
      let placeholderLocalizationKey = metadata->getOptionString("placeholder_localization_key")
      let inputFormatPattern = metadata->getOptionString("input_format_pattern")
      let htmlAutocompleteAttribute = metadata->getOptionString("html_autocomplete_attribute")
      let keyboardType = metadata->getOptionString("keyboard_type")
      let maxInputLength = metadata->getOptionInt("max_input_length")
      let layoutRowId = metadata->getOptionString("layout_row_id")
      let layoutWidthRatio = metadata->getOptionFloat("layout_width_ratio")
      let merchantProvidedDisplayName = metadata->getOptionString("merchant_provided_display_name")
      let merchantProvidedPlaceholderText =
        metadata->getOptionString("merchant_provided_placeholder_text")

      Some({
        intentDataReadPath,
        defaultLabelText,
        defaultPlaceholderText,
        fieldRenderType: fieldRenderTypeStr->stringToFieldType,
        fieldDisplayOrder,
        isRequired,
        dropdownOptions,
        confirmRequestWritePath,
        renderWhenPrefilled,
        validationRuleType,
        validationRegexPattern,
        labelLocalizationKey,
        placeholderLocalizationKey,
        inputFormatPattern,
        htmlAutocompleteAttribute,
        keyboardType,
        maxInputLength,
        layoutRowId,
        layoutWidthRatio,
        merchantProvidedDisplayName,
        merchantProvidedPlaceholderText,
      })
    } else {
      None
    }
  })
}
