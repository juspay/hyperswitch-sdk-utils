open SuperpositionTypes

type sdkPropsConfigService = {
  evaluateConfig: sdkPropsContext => Dict.t<JSON.t>,
}

@module("./../superposition/superposition.js") @new
external cacReader: JSON.t => Nullable.t<sdkPropsConfigService> = "CacReader"

let useSdkPropsResolver = (
  ~rawConfigs,
  ~reshape,
) => {
  let service = React.useMemo(() => {
    switch rawConfigs {
    | None => None
    | Some(configData) => cacReader(configData)->Nullable.toOption
    }
  }, [rawConfigs])

  React.useCallback((context: sdkPropsContext) => {
    switch service {
    | None => Dict.make()
    | Some(svc) =>
      try {
        svc.evaluateConfig(context)->reshape
      } catch {
      | _ => Dict.make()
      }
    }
  }, [service])
}

let useSdkPropsDefaults = (~rawConfigs) =>
  useSdkPropsResolver(~rawConfigs, ~reshape=resolved =>
    SdkPropsHelper.buildNestedConfigFromResolved(resolved)
  )

let useMobileSdkPropsDefaults = (~rawConfigs) =>
  useSdkPropsResolver(~rawConfigs, ~reshape=SdkPropsHelper.buildMobileNestedConfigFromResolved)
