open SuperpositionHelper

type configurationService = {
  evaluateConfig: SuperpositionTypes.superpositionContext => Dict.t<JSON.t>,
}

external importJSON: string => promise<JSON.t> = "import"
@module("./../superposition/superposition.js") @new
external cacReader: JSON.t => Nullable.t<configurationService> = "CacReader"

let service = ref(None)

let useConfigurationService = () => {
  let (nativeProp, _) = React.useContext(NativePropContext.nativePropContext)

  React.useEffect1(() => {
    let loadFallbackService = async () => {
      try {
        let s3Path = "assets/v2/configs/superposition.config.json"
        let configData = await importJSON(`./../../${s3Path}`)
        cacReader(configData)->Nullable.toOption
      } catch {
      | _ex => None
      }
    }

    let initializeService = async () => {
      switch nativeProp.superpositionConfig.configJson {
      | Some(nativeConfig) =>
        let nativeService = try {
          cacReader(nativeConfig)->Nullable.toOption
        } catch {
        | _ex => None
        }
        switch nativeService {
        | Some(_) => service := nativeService
        | None =>
          let fallbackService = await loadFallbackService()
          switch fallbackService {
          | Some(_) => service := fallbackService
          | None =>
            if service.contents->Option.isNone {
              service := None
            }
          }
        }
      | None =>
        let fallbackService = await loadFallbackService()
        switch fallbackService {
        | Some(_) => service := fallbackService
        | None =>
          if service.contents->Option.isNone {
            service := None
          }
        }
      }
    }
    initializeService()->ignore
    None
  }, [nativeProp.superpositionConfig.configJson])

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
