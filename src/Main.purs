module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

import Plotly.AxisLayout (defaultAxisLayout, withGridcolor, withShowgrid, withTitle, withZeroline)
import Plotly.DivId (DivId(..))
import Plotly.Font (Font(..))
import Plotly.Layout (Layout(..))
import Plotly.Legend (Legend(Legend))
import Plotly.Line (Line(..))
import Plotly.Margin (Margin(..))
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
          , marker: defaultMarker # withColor "steelblue"
          , line: Just $ Line { shape: "spline" }
          }
        ]
      layout =
        Layout
        { autosize: Just true
        , font: Just $ Font { family: "Helvetica, sans-serif", size: 14, color: "#333" }
        , legend: Just $ Legend { orientation: "h", x: 1.0, y: 1.1 }
        , margin: Just $ Margin { l: 40, r: 40, t: 60, b: 40 }
        , showlegend: Just true
        , title: Just "Hello PureScript + Plotly"
        , xaxis: Just $ defaultAxisLayout
          # withShowgrid false
          # withTitle "Time"
          # withZeroline false
        , yaxis: Just $ defaultAxisLayout
          # withGridcolor "#eee"
          # withTitle "Membership"
        }
  newPlot (DivId "app") d layout
