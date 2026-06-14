external importJSON: string => promise<JSON.t> = "import"

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
  ~cacheKey: string,
  ~logOutcome: string => unit=_ => (),
) => {
  let (result, setResult) = React.useState(() => emptyResult)

  React.useEffect1(() => {
    let cancelled = ref(false)
    let commitFromFull = fullJson =>
      if !cancelled.contents {
        setResult(_ => {
          rawConfigs: Some(stripRawConfigs(fullJson)),
          paymentMethods: SuperpositionPaymentMethodsType.parsePaymentMethods(fullJson),
        })
      }

    let useBundle = (~reason) =>
      importJSON("../../assets/v2/configs/superposition.config.json")
      ->Promise.then(json => {
        commitFromFull(json)
        logOutcome("bundle:" ++ reason)
        Promise.resolve()
      })
      ->Promise.catch(_ => {
        logOutcome("bundle-unavailable:" ++ reason)
        Promise.resolve()
      })

    switch fetchConfig {
    | Some(fetchConfig) =>
      fetchConfig()
      ->Promise.then(json =>
        if isValidRawConfigs(stripRawConfigs(json)) {
          commitFromFull(json)
          logOutcome("api")
          Promise.resolve()
        } else {
          useBundle(~reason="invalid-api")
        }
      )
      ->Promise.catch(_ => useBundle(~reason="api-error"))
      ->ignore
    | None => useBundle(~reason="no-profile")->ignore
    }

    Some(() => cancelled := true)
  }, [cacheKey])

  result
}
