open SharedLoggerEvents.NativeSdk
module Make = (
  Host: {
    type eventType
    let logState: (~event: stateEvent, ~durationMs: float=?) => unit
    let completeAndValid: eventType => bool
    let fieldCompleteAndValid: (eventType, string) => bool
    let logCompleted: bool => unit
  },
) => {
  // Log field names and completion only. Never retain card data or provider payloads.
  let useFieldLogger = (~elementType: string) => {
    let mountedAt = React.useRef(None)
    let rendered = React.useRef(false)

    let ensureMounted = () =>
      if mountedAt.current->Option.isNone {
        mountedAt.current = Some(Date.now())
        Host.logState(~event=CardFieldMounted({scope: VaultForm, field: elementType}))
      }

    React.useEffect1(() => {
      ensureMounted()
      Some(
        () => {
          Host.logState(~event=CardFieldUnmounted({scope: VaultForm, field: elementType}))
          mountedAt.current = None
          rendered.current = false
        },
      )
    }, [elementType])

    () => {
      ensureMounted()
      if !rendered.current {
        rendered.current = true
        Host.logState(
          ~event=CardFieldRendered({scope: VaultForm, field: elementType}),
          ~durationMs=?mountedAt.current->Option.map(start => Date.now() -. start),
        )
      }
    }
  }

  let useFormLogger = (~formId: string, ~savedMethod=false) => {
    let completed = React.useRef(false)

    React.useEffect1(() => {
      completed.current = false
      Host.logState(~event=CardFormCreated({scope: VaultForm}))
      Some(() => Host.logState(~event=CardFormDestroyed({scope: VaultForm})))
    }, [formId])

    (event: Host.eventType) =>
      if Host.completeAndValid(event) && !completed.current {
        completed.current = true
        let fields =
          ["cardNumber", "cardExpiry", "cardCvc", "cardholderName"]->Array.filter(field =>
            Host.fieldCompleteAndValid(event, field)
          )
        Host.logState(~event=CardFormCompleted({scope: VaultForm, fields}))
        Host.logCompleted(savedMethod)
      }
  }
}
