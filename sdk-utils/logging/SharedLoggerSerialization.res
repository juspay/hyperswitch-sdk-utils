// Wire-only projection: keep typed rows/context mutable and complete in memory.
// Only the protocol's value string is parsed; arbitrary detail strings stay strings.
let serializeRow: SharedLoggerTypes.row => string = %raw(`
  function (row) {
    function clean(value) {
      if (Array.isArray(value)) return value.map(clean);
      if (value !== null && typeof value === "object") {
        var result = {};
        Object.keys(value).forEach(function (key) {
          if (value[key] !== "") {
            Object.defineProperty(result, key, {
              value: clean(value[key]), enumerable: true, writable: true, configurable: true
            });
          }
        });
        return result;
      }
      return value;
    }
    var wire = clean(row);
    if (typeof row.value === "string" && row.value !== "") {
      try {
        wire.value = JSON.stringify(clean(JSON.parse(row.value)));
      } catch (_) {
        // Preserve malformed/non-JSON strings; do not invent or discard their content.
        wire.value = row.value;
      }
    }
    return JSON.stringify(wire);
  }
`)
