module Form = {
  type formMethods = {
    reset: unit => unit,
    submit: unit => unit,
  }

  type formProps = {
    handleSubmit: ReactEvent.Form.t => unit,
    form: formMethods,
    values: Dict.t<JSON.t>,
    errors: Dict.t<string>,
    touched: Dict.t<bool>,
    dirty: bool,
    pristine: bool,
    valid: bool,
    invalid: bool,
    submitting: bool,
  }

  @module("react-final-form") @react.component
  external make: (
    ~onSubmit: Dict.t<string> => unit=?,
    ~validate: option<Dict.t<string> => Dict.t<string>>=?,
    ~initialValues: option<Dict.t<JSON.t>>=?,
    ~render: formProps => React.element,
  ) => React.element = "Form"
}

module Field = {
  type fieldState = {
    value: option<string>,
    error: option<string>,
    touched: bool,
    active: bool,
    dirty: bool,
    invalid: bool,
    pristine: bool,
    valid: bool,
  }

  type inputProps = {
    name: string,
    value: option<string>,
    onChange: string => unit,
    onBlur: unit => unit,
    onFocus: unit => unit,
  }

  type fieldProps = {
    input: inputProps,
    meta: fieldState,
  }

  @module("react-final-form") @react.component
  external make: (
    ~name: string,
    ~validate: option<option<string> => option<string>>=?,
    ~children: fieldProps => React.element,
  ) => React.element = "Field"
}

let createSubmitHandler = (onSubmit: option<Dict.t<string> => unit>) => {
  React.useCallback((values: Dict.t<string>) => {
    switch onSubmit {
    | Some(submitFn) => submitFn(values)
    | None => ()
    }
  }, [onSubmit])
}

let useFormStateHandler = (
  ~onFormChange: Dict.t<JSON.t> => unit,
  ~onValidationChange: bool => unit,
  ~formProps: Form.formProps,
) => {
  React.useEffect2(() => {
    onFormChange(formProps.values)
    onValidationChange(formProps.valid)
    None
  }, (formProps.values, formProps.valid))
}

type useFieldConfig<'a> = {
  afterSubmit?: unit => unit,
  allowNull?: bool,
  beforeSubmit?: unit => option<bool>,
  component?: React.component<'a>,
  data?: Dict.t<'a>,
  defaultValue?: 'a,
  format?: ('a, string) => 'a,
  formatOnBlur?: bool,
  initialValue?: 'a,
  isEqual?: ('a, 'a) => bool,
  multiple?: bool,
  parse?: ('a, string) => 'a,
  type_?: string,
  validate?: 'a => option<string>,
  validateFields?: array<string>,
  value?: 'a,
}

@module("react-final-form")
external useField: (string, ~config: useFieldConfig<'a>=?, unit) => Field.fieldProps = "useField"
