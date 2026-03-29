open SuperpositionHelper

type configurationService = {
  evaluateConfig: SuperpositionTypes.superpositionContext => Dict.t<JSON.t>,
}

external importJSON: string => promise<JSON.t> = "import"
@module("./../superposition/superposition.js") @new
external cacReader: JSON.t => Nullable.t<configurationService> = "CacReader"

let extractRawConfigs = (config: JSON.t): JSON.t => {
  switch config->JSON.Decode.object {
  | Some(dict) =>
    switch dict->Dict.get("raw_configs")->Option.flatMap(JSON.Decode.object) {
    | Some(rawConfigs) => rawConfigs->JSON.Encode.object
    | None => config
    }
  | None => config
  }
}

let extractContextUsed = (config: JSON.t): (string, string, string) => {
  switch config->JSON.Decode.object {
  | Some(dict) =>
    switch dict->Dict.get("context_used")->Option.flatMap(JSON.Decode.object) {
    | Some(contextDict) => (
        contextDict->Dict.get("profile_id")->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
        contextDict->Dict.get("merchant_id")->Option.flatMap(JSON.Decode.string)->Option.getOr(""),
        contextDict
        ->Dict.get("organization_id")
        ->Option.flatMap(JSON.Decode.string)
        ->Option.getOr(""),
      )
    | None => ("", "", "")
    }
  | None => ("", "", "")
  }
}

let service: ref<option<(configurationService, string, string, string)>> = ref(None)

let useConfigurationService = () => {
  let (nativeProp, _) = React.useContext(NativePropContext.nativePropContext)

  React.useEffect1(() => {
    let loadFallbackService = async () => {
      try {
        let s3Path = "assets/v2/configs/superposition.config.json"
        let configData = await importJSON(`./../../${s3Path}`)
        let (profileId, merchantId, organizationId) = extractContextUsed(configData)
        cacReader(extractRawConfigs(configData))
        ->Nullable.toOption
        ->Option.map(svc => (svc, profileId, merchantId, organizationId))
      } catch {
      | _ex => None
      }
    }

    let initializeService = async () => {
      switch nativeProp.hyperParams.superpositionConfigRaw {
      | Some(nativeConfig) =>
        let nativeService = try {
          let (profileId, merchantId, organizationId) = extractContextUsed(nativeConfig)
          cacReader(extractRawConfigs(nativeConfig))
          ->Nullable.toOption
          ->Option.map(svc => (svc, profileId, merchantId, organizationId))
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
  }, [nativeProp.hyperParams.superpositionConfigRaw])

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
    | (Some((svc, profileId, merchantId, organizationId)), false) =>
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
              profile_id: profileId,
              merchant_id: merchantId,
              organization_id: organizationId,
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
