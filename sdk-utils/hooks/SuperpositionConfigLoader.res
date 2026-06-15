let stripRawConfigs = json =>
  json
  ->JSON.Decode.object
  ->Option.flatMap(dict => dict->Dict.get("raw_configs"))
  ->Option.getOr(json)

let isValidRawConfigs = json =>
  json
  ->JSON.Decode.object
  ->Option.map(dict =>
    dict->Dict.get("default_configs")->Option.isSome || dict->Dict.get("contexts")->Option.isSome
  )
  ->Option.getOr(false)

type superpositionConfigResult = {
  rawConfigs: option<JSON.t>,
  paymentMethods: option<SuperpositionPaymentMethodsType.superpositionPaymentMethods>,
}

let emptyResult = {rawConfigs: None, paymentMethods: None}

let useSuperpositionRawConfigs = (
  ~fetchConfig: option<unit => promise<JSON.t>>,
  ~refetchKey: string,
  ~logOutcome: string => unit=_ => (),
) => {
  let (result, setResult) = React.useState(() => emptyResult)

  React.useEffect1(() => {
    let cancelled = ref(false)

    // Config is profile-level — fetched ONLY from the API. No bundled fallback:
    // a hardcoded one-profile bundle would be wrong for any other merchant.
    switch fetchConfig {
    | Some(fetchConfig) =>
      fetchConfig()
      ->Promise.then(json => {
        if isValidRawConfigs(stripRawConfigs(json)) {
          if !cancelled.contents {
            setResult(_ => {
              rawConfigs: Some(stripRawConfigs(json)),
              paymentMethods: SuperpositionPaymentMethodsType.parsePaymentMethods(json),
            })
          }
          logOutcome("api")
        } else {
          logOutcome("invalid-api")
        }
        Promise.resolve()
      })
      ->Promise.catch(_ => {
        logOutcome("api-error")
        Promise.resolve()
      })
      ->ignore
    | None => logOutcome("no-profile")
    }

    Some(() => cancelled := true)
  }, [refetchKey])

  result
}
