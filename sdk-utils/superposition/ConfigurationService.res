open Promise
type configurationService
type resolvedConfig = Dict.t<JSON.t>
type context = {
  connector: string,
  payment_method: string,
  payment_method_type: option<string>,
  country: option<string>,
  mandate_type: option<string>,
}

@send external evaluateConfig: (configurationService, context) => resolvedConfig = "evaluateConfig"

@val
external fetch: string => Promise.t<'response> = "fetch"

@send external json: 'response => Promise.t<JSON.t> = "json"

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
  fetch("config.json")
  ->then(res => res->json)
  ->then(json => {
    service.configData = Some(json)
    let cacReader = %raw(`new (require("./superposition.js")).CacReader(json)`)
    service.cacClient = Some(cacReader)
    Console.log("SuperpositionClient initialized successfully")
    resolve(true)
  })
  ->catch(_err => {
    Console.error2("Error loading configuration:", _err)
    resolve(false)
  })
}

let evaluateConfiguration = (service, context): option<resolvedConfig> => {
  switch service.cacClient {
  | None =>
    Console.error("Configuration service not initialized")
    None
  | Some(client) =>
    try {
      Console.log("Evaluating context:")
      let resolvedConfig = client->evaluateConfig(context)
      Console.log("Resolved config:")
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
