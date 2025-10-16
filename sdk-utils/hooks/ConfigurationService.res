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
          let s3Path = "assets/v2/configs/superposition.config.json"
          let configData = try {
            let response = await Fetch.fetch(`https://checkout.hyperswitch.io/${s3Path}`)
            await response->Fetch.Response.json
          } catch {
          | _ => await importJSON(`./../../${s3Path}`)
          }

          let configService = cacReader(configData)->Nullable.toOption
          service := configService
        } catch {
        | _ex => service := None
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
              paymentMethod: configParams.paymentMethod->CommonUtils.snakeToPascalCase,
              paymentMethodType: configParams.paymentMethodType->CommonUtils.snakeToPascalCase,
              country: configParams.country,
              mandateType: configParams.mandateType,
              collectBillingDetailsFromWalletConnector: configParams.collectBillingDetailsFromWalletConnector,
              collectShippingDetailsFromWalletConnector: configParams.collectShippingDetailsFromWalletConnector,
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
