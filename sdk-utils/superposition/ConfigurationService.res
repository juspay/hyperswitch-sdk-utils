open Promise
type configurationService
type resolvedConfig = Dict.t<JSON.t>

type context = {
  connector: string,
  payment_method: string,
  payment_method_type: option<string>,
  country: option<string>,
  mandate_type: option<string>,
  collect_shipping_details_from_wallet_connector: option<string>,
  collect_billing_details_from_wallet_connector: option<string>,
}

@send external evaluateConfig: (configurationService, context) => resolvedConfig = "evaluateConfig"

let configData = %raw(`require('./config.json')`)

type t = {
  mutable cacClient: option<configurationService>,
  mutable configData: option<JSON.t>,
}

type superpositionModule = {"CacReader": configurationService}

let make = {
  {
    cacClient: None,
    configData: None,
  }
}

let initialize = (service): Promise.t<bool> => {
  try {
    service.configData = Some(configData)
    let cacReader = %raw(`new (require("./superposition.js")).CacReader(configData)`)
    service.cacClient = Some(cacReader)
    resolve(true)
  } catch {
  | _err => {
      Console.error2("Error loading configuration:", _err)
      resolve(false)
    }
  }
}

let evaluateConfiguration = (service, context): option<resolvedConfig> => {
  switch service.cacClient {
  | None =>
    Console.error("Configuration service not initialized")
    None
  | Some(client) =>
    try {
      let resolvedConfig = client->evaluateConfig(context)
      Some(resolvedConfig)
    } catch {
    | e =>
      Console.error2("Error evaluating configuration:", e)
      None
    }
  }
}

let isInitialized = (service): bool => service.cacClient !== None

let getConfigData = (service): option<JSON.t> => service.configData

let reset = (service): unit => {
  service.cacClient = None
  service.configData = None
}

let configurationService = make
