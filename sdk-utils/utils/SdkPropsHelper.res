let sdkPropsConfigPrefix = "checkout_sdk."

let rec setValueAtPath = (dict, segments, value) => {
  switch segments->Array.length {
  | 0 => ()
  | 1 =>
    let key = segments->Array.getUnsafe(0)
    if key !== "" {
      dict->Dict.set(key, value)
    }
  | _ =>
    let key = segments->Array.getUnsafe(0)
    if key !== "" {
      let rest = segments->Array.sliceToEnd(~start=1)
      let child = switch dict->Dict.get(key)->Option.flatMap(JSON.Decode.object) {
      | Some(obj) => obj
      | None => Dict.make()
      }
      setValueAtPath(child, rest, value)
      dict->Dict.set(key, child->JSON.Encode.object)
    }
  }
}

let buildNested = (
  resolved,
  ~mapEntry,
) => {
  let prefix = sdkPropsConfigPrefix
  let result = Dict.make()
  resolved
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    if key->String.startsWith(prefix) && !(key->String.includes("[]")) {
      switch value->JSON.Classify.classify {
      | Null => () 
      | _ =>
        switch mapEntry(key->String.sliceToEnd(~start=prefix->String.length), value) {
        | Some((path, mappedValue)) => setValueAtPath(result, path->String.split("."), mappedValue)
        | None => () 
        }
      }
    }
  })
  result
}

let buildNestedConfigFromResolved = (resolved) =>
  buildNested(resolved, ~mapEntry=(path, value) => Some((path, value)))

let buildMobileNestedConfigFromResolved = (resolved) =>
  buildNested(resolved, ~mapEntry=SdkPropsRoleMap.forCanonicalPath)
