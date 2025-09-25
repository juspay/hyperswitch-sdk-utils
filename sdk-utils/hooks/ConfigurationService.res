open SuperpositionHelper

type configurationService = {
  evaluateConfig: SuperpositionTypes.superpositionContext => Dict.t<JSON.t>,
}

external importJSON: string => promise<JSON.t> = "import"
@module("./../superposition/superposition.js") @new
external cacReader: JSON.t => Nullable.t<configurationService> = "CacReader"

let service = ref(None)

let useConfigurationService = () => {
  React.useEffect0(() => {
    let initializeService = async () => {
      if service.contents->Option.isNone {
        try {
          let configData = try {
            let headers = Dict.make()
            headers->Dict.set("Accept-Encoding", "br, gzip")
            let response = await Fetch.fetch(
              "https://checkout.hyperswitch.io/assets/v1/configs/superposition.config.json",
            )
            await response->Fetch.Response.json
          } catch {
          | err =>
            Console.log(err)
            await importJSON("./../superposition/config.json")
          }

          let configService = cacReader(configData)->Nullable.toOption
          service.contents = configService
        } catch {
        | _ex => service.contents = None
        }
      }
    }
    initializeService()->ignore
    None
  })

  (
    eligibleConnectors: array<RescriptCore.JSON.t>,
    configParams: SuperpositionTypes.superpositionBaseContext,
    requiredFieldsFromPML,
  ) => {
    let requiredFieldsFromSuperPosition = switch (
      service.contents,
      Array.length(eligibleConnectors) === 0,
    ) {
    | (_, true)
    | (None, false) => []
    | (Some(svc), false) =>
      eligibleConnectors
      ->removeDuplicateConnectors
      ->Array.reduce([], (acc, connector) => {
        try {
          switch connector->JSON.Decode.string {
          | Some("") | None => ()
          | Some(connector) =>
            let transformedContext: SuperpositionTypes.superpositionContext = {
              connector: connector->CommonUtils.snakeToPascalCase,
              payment_method: configParams.payment_method->CommonUtils.snakeToPascalCase,
              payment_method_type: configParams.payment_method_type->CommonUtils.snakeToPascalCase,
              country: configParams.country,
              mandate_type: configParams.mandate_type,
              collect_billing_details_from_wallet_connector: configParams.collect_billing_details_from_wallet_connector,
              collect_shipping_details_from_wallet_connector: configParams.collect_shipping_details_from_wallet_connector,
            }
            let resolvedConfig =
              svc.evaluateConfig(transformedContext)->convertConfigurationToRequiredFields
            acc->Array.pushMany(resolvedConfig)
          }
        } catch {
        | _ex => ()
        }
        acc
      })
      ->removeShippingAndDuplicateFields
      ->sortFieldsByPriorityOrder
    }

    let missingRequiredFields = filterFieldsBasedOnMissingData(
      requiredFieldsFromSuperPosition,
      requiredFieldsFromPML,
    )

    let initialValues = convertFlatDictToNestedObject(requiredFieldsFromPML)

    (requiredFieldsFromSuperPosition, missingRequiredFields, initialValues)
  }
}
