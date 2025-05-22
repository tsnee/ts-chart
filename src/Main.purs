module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

import Plotly.AxisLayout (AxisLayout(..))
import Plotly.DivId (DivId(DivId))
import Plotly.Layout (Layout(..))
import Plotly.Marker (defaultMarker, withColor)
import Plotly.Plotly (newPlot)
import Plotly.TraceData (TraceData(..))

main :: Effect Unit
main = do
  let d =
        [ TraceData
          { x: [1, 2, 3, 4]
          , y: [10, 15, 13, 17]
          , type: "scatter"
          , mode: "lines+markers"
          , marker: defaultMarker # withColor "blue"
          }
        ]
      layout =
        Layout
        { title: Just "Hello PureScript + Plotly"
        , xaxis: Just $ AxisLayout { title: Just "Time" }
        , yaxis: Just $ AxisLayout { title: Just "Membership" }
        }
  newPlot (DivId "app") d layout
