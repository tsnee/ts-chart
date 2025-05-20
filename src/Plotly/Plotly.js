import Plotly from 'plotly.js-dist'

export function _newPlot(divId, data, layout) {
  Plotly.newPlot(divId, data, layout)
}
