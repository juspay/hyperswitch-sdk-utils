open SuperpositionHelper

external importJSON: string => promise<JSON.t> = "import"
@module("./superposition.js") @new
external cacReader: JSON.t => Nullable.t<configurationService> = "CacReader"

let service = ref(None)

let useConfigurationService = () => {
  React.useEffect0(() => {
    let initializeService = async () => {
      try {
        let configData = await importJSON("./config.json")
        let configService = cacReader(configData)->Nullable.toOption
        service.contents = configService
      } catch {
      | _ex => ()
      }
    }
    if service.contents->Option.isNone {
      initializeService()->ignore
    }
    None
  })

  let getResolvedConfig = (context: SuperpositionTypes.superpositionContext) => {
    switch service.contents {
    | None => None
    | Some(svc) =>
      try {
        let newContext = {
          ...context,
          connector: context.connector->CommonUtils.snakeToPascalCase,
          payment_method: context.payment_method->CommonUtils.snakeToPascalCase,
          payment_method_type: context.payment_method_type->CommonUtils.snakeToPascalCase,
        }
        Console.log2("aaa", newContext)
        let resolvedConfig = svc.evaluateConfig(newContext)
        Console.log2("xyz", resolvedConfig)
        Some(resolvedConfig)
      } catch {
      | _ex => None
      }
    }
  }

  (context: SuperpositionTypes.connectorArrayContext) => {
    let combinedRequiredFieldsFromAllConnectors = []
    let {
      eligibleConnectors,
      payment_method,
      payment_method_type,
      country,
      mandate_type,
      collect_shipping_details_from_wallet_connector,
      collect_billing_details_from_wallet_connector,
    } = context

    eligibleConnectors->Array.forEach(connector => {
      let context: SuperpositionTypes.superpositionContext = {
        connector,
        payment_method,
        payment_method_type,
        country,
        mandate_type,
        collect_shipping_details_from_wallet_connector,
        collect_billing_details_from_wallet_connector,
      }

      let resolvedConfig = getResolvedConfig(context)

      let fields = resolvedConfig->Option.map(parseResolvedConfigToFields)
      let requiredFields = fields->Option.map(filterRequiredFields)
      let _ =
        requiredFields->Option.map(fields =>
          fields->Array.forEach(field => combinedRequiredFieldsFromAllConnectors->Array.push(field))
        )
    })

    combinedRequiredFieldsFromAllConnectors
    ->removeDuplicateByOutputPath
    ->groupFieldsByComponentAndSortByPriority
  }
}
