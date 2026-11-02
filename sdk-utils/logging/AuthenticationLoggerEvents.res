open SharedLoggerTypes

type transStatusData = {transStatus: string}
type threeDsMethodFailure = MissingContainer | FormSubmitFailed | IframeLoadFailed
type threeDsMethodFailureData = {reason: threeDsMethodFailure}
type threeDsPopupFailure = MessageHandlingFailed
type threeDsPopupFailureData = {reason: threeDsPopupFailure}

type userEvent = ThreeDsPopupDismissed

let userSeverity = (_: userEvent) => Info

type apiEvent = Authentication

let apiSeverity = (_: apiEvent) => defaultSeverity

type lifecycleEvent =
  | ThreeDsChallengeShown(transStatusData)
  | ThreeDsFrictionlessResolved(transStatusData)
  | ThreeDsAuthContainerMissing(transStatusData)
  | ThreeDsAuthRequestFailed
  | ThreeDsMethodStarted
  | ThreeDsMethodCompleted
  | ThreeDsMethodSkipped
  | ThreeDsMethodFailed(threeDsMethodFailureData)
  | ThreeDsMethodTimedOut
  | ThreeDsPopupRequested
  | ThreeDsPopupFailed(threeDsPopupFailureData)

let lifecycleSeverity = event =>
  switch event {
  | ThreeDsMethodStarted
  | ThreeDsMethodCompleted
  | ThreeDsMethodSkipped
  | ThreeDsPopupRequested =>
    Debug
  | ThreeDsChallengeShown(_) | ThreeDsFrictionlessResolved(_) => Info
  | ThreeDsMethodTimedOut => Warning
  | ThreeDsAuthContainerMissing(_) | ThreeDsAuthRequestFailed | ThreeDsPopupFailed(_) => Error
  | ThreeDsMethodFailed({reason}) =>
    switch reason {
    | MissingContainer | FormSubmitFailed => Error
    | IframeLoadFailed => Warning
    }
  }
