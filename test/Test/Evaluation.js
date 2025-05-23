export function _evaluate(x, s) {
  return function() {
    try {
      return { actual: JSON.stringify(x), expected: JSON.stringify(JSON.parse(s)) };
    } catch (e) {
      return { actual: JSON.stringify(e), expected: s }; // Handles invalid JSON strings
    }
  }
}
