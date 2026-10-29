module type NativeHost = {
  let endpoint: unit => string
  let headers: unit => Dict.t<string>
  let timeoutMs: int
}
module Native = (Host: NativeHost) => {
  external asObject: Dict.t<string> => {..} = "%identity"
  let sendPayload = payload =>
    switch Host.endpoint() {
    | "" => Promise.resolve(false)
    | endpoint =>
      let controller = Fetch.AbortController.make()
      let deadline = setTimeout(() => controller->Fetch.AbortController.abort, Host.timeoutMs)
      Fetch.fetch(
        endpoint,
        (
          {
            method: #POST,
            body: Fetch.Body.string(payload),
            headers: Fetch.Headers.fromObject(Host.headers()->asObject),
            mode: #"no-cors",
            signal: controller->Fetch.AbortController.signal,
          }: Fetch.Request.init
        ),
      )
      ->Promise.then(response => {
        clearTimeout(deadline)
        // no-cors responses are opaque (status 0) on web
        Promise.resolve(response->Fetch.Response.ok || response->Fetch.Response.status === 0)
      })
      ->Promise.catch(_ => {
        clearTimeout(deadline)
        Promise.resolve(false)
      })
    }
}
module type WebHost = {
  let endpoint: unit => string
  let hasSendBeacon: unit => bool
  let sendBeacon: (string, string) => bool
}
module Web = (Host: WebHost) => {
  let sendPayload = payload =>
    Host.endpoint() !== "" && Host.hasSendBeacon()
      ? try Host.sendBeacon(Host.endpoint(), payload) catch {
        | _ => false
        }
      : false
}
