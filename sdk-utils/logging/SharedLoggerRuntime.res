// Only runtime primitives with identical behavior across the two hosts.
// Initialization, event emission, context, transport and rethrow stay local.
open SharedLoggerTypes

type browserInfo = {name: string, version: string}

let detectBrowser: unit => browserInfo = %raw(`
  function () {
    var ua = typeof navigator !== "undefined" ? navigator.userAgent || "" : "";
    var rules = [
      [/Instagram[\/ ]([\d.]+)/, "Instagram"],
      [/FBAV\/([\d.]+)/, "Facebook"],
      [/LinkedInApp\/([\d.]+)/, "LinkedIn"],
      [/Edg(?:e|A|iOS)?\/([\d.]+)/, "Edge"],
      [/(?:OPR|Opera)\/([\d.]+)/, "Opera"],
      [/SamsungBrowser\/([\d.]+)/, "Samsung Internet"],
      [/FxiOS\/([\d.]+)/, "Mobile Firefox"],
      [/(?:Mobile|Tablet);.*Firefox\/([\d.]+)/, "Mobile Firefox"],
      [/Firefox\/([\d.]+)/, "Firefox"],
      [/CriOS\/([\d.]+)/, "Mobile Chrome"],
      [/ wv\).+Chrome\/([\d.]+)/, "Chrome WebView"],
      [/Chrome\/([\d.]+) Mobile/, "Mobile Chrome"],
      [/Chrome\/([\d.]+)/, "Chrome"],
      [/Version\/([\d.]+).*Mobile\/\w+ Safari/, "Mobile Safari"],
      [/Version\/([\d.]+).*Safari\//, "Safari"],
      [/AppleWebKit\/([\d.]+)/, "WebKit"],
    ];
    for (var i = 0; i < rules.length; i++) {
      var match = rules[i][0].exec(ua);
      if (match) return { name: rules[i][1], version: match[1] };
    }
    return { name: "Others", version: "0" };
  }
`)

let rank = severity =>
  switch severity {
  | Debug => 0
  | Info => 1
  | Warning => 2
  | Error => 3
  }

let step = (~outcome, ~durationMs=?, ~failureClass=?, ~error=?, ~timeoutMs=0) => {
  outcome,
  durationMs,
  failureClass,
  error,
  timeoutMs,
}

type tracker = {
  startedAt: float,
  mutable settled: bool,
  mutable timer: option<timeoutId>,
}

let makeTracker = () => {startedAt: Date.now(), settled: false, timer: None}

let elapsed = tracker => Date.now() -. tracker.startedAt

let settle = tracker =>
  if tracker.settled {
    false
  } else {
    tracker.settled = true
    tracker.timer->Option.forEach(clearTimeout)
    tracker.timer = None
    true
  }

let isThenable: 'value => bool = %raw(`
  (value) => value != null && typeof value.then === "function"
`)

let forwardInvocation: ('fn, (unit => 'value) => 'value) => 'fn = %raw(`
  function (fn, run) {
    if (typeof fn !== "function") {
      return fn;
    }
    var wrapped = function () {
      var self = this;
      var args = arguments;
      return run(function () {
        return fn.apply(self, args);
      });
    };
    try {
      Object.defineProperty(wrapped, "length", { value: fn.length });
    } catch (_) {}
    return wrapped;
  }
`)

type crashEvent = {message?: JSON.t, filename?: JSON.t, reason?: JSON.t}

let describe: option<JSON.t> => string = %raw(`
  (v) => typeof v == "string" ? v : (v && typeof v.message == "string" ? v.message : "UNKNOWN")
`)

let stackUrl: option<JSON.t> => string = %raw(`
  (v) => (v && typeof v.stack == "string" && /(?:https?|blob|file):\/\/[^\s)]+/.exec(v.stack) || [""])[0]
`)
