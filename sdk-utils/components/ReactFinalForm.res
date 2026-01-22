type getState = {values: Dict.t<JSON.t>, valid: bool}

type formMethods = {
  reset: unit => unit,
  submit: unit => unit,
  getState: unit => getState,
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

module Form = {
  @module("react-final-form") @react.component
  external make: (
    ~onSubmit: (Dict.t<string>, formMethods) => unit=?,
    ~validate: option<Dict.t<string> => Dict.t<string>>=?,
    ~initialValues: option<Dict.t<JSON.t>>=?,
    ~render: formProps => React.element,
  ) => React.element = "Form"
}

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

type inputProps<'t> = {
  name: string,
  value: option<string>,
  onChange: string => unit,
  onBlur: 't => unit,
  onFocus: 't => unit,
}

type fieldProps<'t> = {
  input: inputProps<'t>,
  meta: fieldState,
}

type fieldRenderPropsCustomInput<'t> = {
  name: string,
  onBlur: ReactEvent.Focus.t => unit,
  onChange: 't => unit,
  onFocus: ReactEvent.Focus.t => unit,
  value: option<string>,
  checked: bool,
}

module Field = {
  @module("react-final-form") @react.component
  external make: (
    ~name: string,
    ~validate: option<option<string> => option<string>>=?,
    ~children: fieldProps<'t> => React.element,
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
  ~formProps: formProps,
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
external useField: (string, ~config: useFieldConfig<'a>=?) => fieldProps<'t> = "useField"

type formSubscription = JSON.t
module FormSpy = {
  @module("react-final-form") @react.component
  external make: (
    ~children: formProps => React.element,
    ~component: bool=?,
    ~onChange: bool=?,
    ~render: formProps => React.element=?,
    ~subscription: formSubscription,
  ) => React.element = "FormSpy"
}

let useFormSubscription = (keys): formSubscription => {
  React.useMemo(() => {
    let dict = Dict.make()
    keys->Array.forEach(key => {
      Dict.set(dict, key, JSON.Encode.bool(true))
    })
    dict->JSON.Encode.object
  }, [])
}
