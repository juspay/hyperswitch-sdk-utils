/**
 * CURRENT IMPLEMENTATION:
 * We have two similar functions (useConfigurationService and useConfigurationServiceWeb): one is async, and the other is not.
 *
 * - Web: We need to ensure the config is fully loaded before evaluating it.
 * - Native: The setup guarantees that the superposition result is received before
 *   the form renders. Thus, the config is loaded beforehand, eliminating the need
 *   for an async function.
 *
 * TODO (Migration plan):
 * We plan to remove both implementations in the final version of superposition.
 * - Web: The `superposition-config-file` will be received along with `hyperLoader.js`.
 * - Mobile: Config files will be pre-fetched at the native layer, allowing
 *   `bundle.js` to mount alongside the config file.
 */
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
          let configFileName = "superposition.config.json"
          let s3Path = "assets/v2/configs/" ++ configFileName
          let configData = try {
            let response = await Fetch.fetch(
              `${s3Path}`,
              {
                method: #GET,
              },
            )
            await response->Fetch.Response.json
          } catch {
          | _ => await importJSON(`../../assets/v2/configs/${configFileName}`)
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

let useConfigurationServiceWeb = () => {
  let superpositionConfigLoadedPromise = React.useRef(Promise.make((_, _) => {()}))
  React.useEffect0(() => {
    let initializeService = async resolve => {
      if service.contents->Option.isNone {
        try {
          let configFileName = "superposition.config.json"
          let s3Path = "assets/v2/configs/" ++ configFileName
          let configData = try {
            let response = await Fetch.fetch(
              `${s3Path}`,
              {
                method: #GET,
              },
            )
            await response->Fetch.Response.json
          } catch {
          | _ => await importJSON(`../../assets/v2/configs/${configFileName}`)
          }

          let configService = cacReader(configData)->Nullable.toOption
          service := configService
        } catch {
        | _ex => service := None
        }
      }
      resolve()
    }
    superpositionConfigLoadedPromise.current = Promise.make((resolve, _) =>
      initializeService(resolve)->ignore
    )
    None
  })

  async (
    eligibleConnectors: array<RescriptCore.JSON.t>,
    configParams: SuperpositionTypes.superpositionBaseContext,
    requiredFieldsFromPML,
  ) => {
    await superpositionConfigLoadedPromise.current
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
