import Plotly from 'plotly.js-dist'

export function _newPlot(divId, data, layout) {
  return function() {
    Plotly.newPlot(divId, data, layout)
  }
}