open SuperpositionHelper

type configurationService = {
  evaluateConfig: SuperpositionTypes.superpositionContext => Dict.t<JSON.t>,
}

@module("./../superposition/superposition.js") @new
external cacReader: JSON.t => Nullable.t<configurationService> = "CacReader"

let useConfigurationService = (~rawConfigs: option<JSON.t>) => {
  let service = React.useMemo(() => {
    switch rawConfigs {
    | None => None
    | Some(configData) => cacReader(configData)->Nullable.toOption
    }
  }, [rawConfigs])

  React.useCallback(
    (
      eligibleConnectors: array<RescriptCore.JSON.t>,
      configParams: SuperpositionTypes.superpositionBaseContext,
      intentData: JSON.t,
    ) => {
      let intentDataDict = intentData->CommonUtils.getDictFromJson
      let requiredFieldsFromSuperPosition = switch (
        service,
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
                connector,
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
          | _ => ()
          }
          acc
        })
        ->removeShippingAndDuplicateFields
        ->sortFieldsByPriorityOrder
      }

      let missingRequiredFields = filterFieldsBasedOnMissingData(
        requiredFieldsFromSuperPosition,
        intentDataDict,
      )

      let initialValues =
        buildInitialValuesFromIntentData(
          requiredFieldsFromSuperPosition,
          intentDataDict,
        )->convertFlatDictToNestedObject

      (requiredFieldsFromSuperPosition, missingRequiredFields, initialValues)
    },
    [service],
  )
}
