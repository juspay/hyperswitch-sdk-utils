let resolveFieldTexts = (
  ~field: SuperpositionTypes.fieldConfig,
  ~getLocalized: string => option<string>,
): SuperpositionTypes.fieldTexts => {
  let label = switch field.customLabelText {
  | Some(text) if text !== "" => text
  | _ =>
    field.labelLocalizationKey
    ->Option.flatMap(getLocalized)
    ->Option.getOr(field.defaultLabelText)
  }

  let placeholder = switch field.customPlaceholderText {
  | Some(text) if text !== "" => text
  | _ =>
    let defaultPlaceholder =
      field.defaultPlaceholderText->String.length > 0 ? field.defaultPlaceholderText : label
    field.placeholderLocalizationKey
    ->Option.flatMap(getLocalized)
    ->Option.getOr(defaultPlaceholder)
  }

  {label, placeholder}
}
