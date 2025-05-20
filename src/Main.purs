module Main where

import Prelude

import Effect (Effect)
--import Effect.Class.Console (log)

import Plotly.TraceData (TraceData(..))
import Plotly.DivId (DivId(DivId))
import Plotly.Layout (Layout(..))
import Plotly.Marker (Marker(..))
import Plotly.Plotly (newPlot)

main :: Effect Unit
main = do
  let d = [
           TraceData
           { x: [1, 2, 3, 4]
           , y: [10, 15, 13, 17]
           , type: "scatter"
           , mode: "lines+markers"
           , marker: Marker { color: "red" }
           }
  ]
  let layout = Layout { title: "Hello PureScript + Plotly" }
  newPlot (DivId "app") d layout
