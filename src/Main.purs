module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

import Plotly.TraceData (TraceData(..))
import Plotly.DivId (DivId(DivId))
import Plotly.Layout (Layout(..))
import Plotly.Marker (Marker(..), defaultMarker, withColor)
import Plotly.Plotly (newPlot)

main :: Effect Unit
main = do
  let d = [
           TraceData
           { x: [1, 2, 3, 4]
           , y: [10, 15, 13, 17]
           , type: "scatter"
           , mode: "lines+markers"
           , marker: defaultMarker # withColor "blue"
           }
  ]
  let layout = Layout { title: Just "Hello PureScript + Plotly" }
  newPlot (DivId "app") d layout
