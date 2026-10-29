let chainGlobalHandler: ((option<JSON.t>, bool) => unit) => bool = %raw(`
  function (report) {
    var errorUtils = typeof ErrorUtils !== "undefined" ? ErrorUtils : undefined;
    if (!errorUtils || typeof errorUtils.setGlobalHandler !== "function") return false;
    var previous = typeof errorUtils.getGlobalHandler === "function" ? errorUtils.getGlobalHandler() : undefined;
    errorUtils.setGlobalHandler(function (error, isFatal) {
      try { report(error, !!isFatal); } catch (_) {}
      if (typeof previous === "function") previous(error, isFatal);
    });
    return true;
  }
`)

let hasWindowEvents: unit => bool = %raw(`
  function () {
    return typeof window !== "undefined" && typeof window.addEventListener === "function";
  }
`)

let nativeUtf8Length: string => int = %raw(`
  function (text) {
    var bytes = 0;
    for (var i = 0; i < text.length; i++) {
      var code = text.charCodeAt(i);
      if (code < 0x80) bytes += 1;
      else if (code < 0x800) bytes += 2;
      else if (code >= 0xd800 && code < 0xdc00 && i + 1 < text.length) { bytes += 4; i++; }
      else bytes += 3;
    }
    return bytes;
  }
`)

let urlOrigin = url =>
  switch url->String.match(%re("/^[a-z][a-z0-9+.-]*:\/\/[^\/?#]*/i")) {
  | Some([Some(origin)]) => origin
  | _ => ""
  }

let detectHref: unit => string = %raw(`
  function () {
    var location = typeof window !== "undefined" && window.location;
    return location && location.origin ? location.origin + (location.pathname || "") : "";
  }
`)

let makeWebByteCounter: unit => string => int = %raw(`function () { var encoder = new TextEncoder(); return function (text) { return encoder.encode(text).length; }; }`)
