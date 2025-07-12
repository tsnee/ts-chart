import Plotly from 'plotly.js-dist'

export function _newPlot(divId, data, layout) {
  return function() {
    return Plotly.newPlot(divId, data, layout)
  }
}

export function _updatePlot(plot, data, layout) {
  return function() {
    return Plotly.react(plot, data, layout)
  }
}
