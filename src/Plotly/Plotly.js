import Plotly from 'plotly.js-dist'

export function _newPlot(divId, data, layout) {
  console.log(layout)
  Plotly.newPlot(divId, data, layout)
}
