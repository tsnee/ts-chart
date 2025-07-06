export function _evaluate(jsObject, jsonString) {
  return function() {
    try {
      return { actual: JSON.stringify(jsObject), expected: JSON.stringify(JSON.parse(jsonString)) };
    } catch (e) {
      return { actual: JSON.stringify(e), expected: jsonString }; // Handles invalid JSON strings
    }
  }
}
