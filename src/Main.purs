module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)

import Plotly.AxisLayout (defaultAxisLayout, withGridcolor, withShowgrid, withZeroline)
import Plotly.AxisLayout (withTitle) as AL
import Plotly.ColorScale (ColorScale(..))
import Plotly.DivId (DivId(..))
import Plotly.Font (Font(..))
import Plotly.Layout (Layout(..), defaultLayout, withTitle, withXAxis, withYAxis) as L
import Plotly.Legend (Legend(Legend))
import Plotly.Line (Shape(..), defaultLine, withColor, withShape)
import Plotly.Margin (Margin(..))
import Plotly.Marker (defaultMarker, withColor) as Marker
import Plotly.Plotly (newPlot)
import Plotly.TraceData (smartTraceData, withColorscale, withFill, withFillcolor, withLine, withMarker, withMode, withName, withShowscale, withZ)
import Data.List.Types (List(Nil), (:))

main :: Effect Unit
main = do
  let x = [1, 2, 3, 4]
      y = [10, 15, 13, 17]
      typ = "scatter"
      td0 =
        [
          smartTraceData x y typ
            # withName "Line Chart Data"
            # withLine (defaultLine # withColor "#004165" # withShape Linear)
            # withMarker (Marker.defaultMarker # Marker.withColor "#004165")
            # withMode "lines+markers"
            # withFill "tozeroy"
            # withFillcolor "#004165"
        ]
      layout0 =
        L.Layout
        { autosize: Just true
        , barmode: Nothing
        , font: Just $ Font { family: "Source Sans 3, sans-serif", size: 16, color: "#772432" }
        , legend: Just $ Legend { orientation: "h", x: 1.0, y: 1.1 }
        , margin: Just $ Margin { l: 40, r: 40, t: 60, b: 40 }
        , showlegend: Just true
        , title: Just "Line Chart Title"
        , xaxis: Just $ defaultAxisLayout
          # withShowgrid false
          # AL.withTitle "Time"
          # withZeroline false
        , yaxis: Just $ defaultAxisLayout
          # withGridcolor "#eee"
          # AL.withTitle "DCP Points"
        }
  newPlot (DivId "linechart") td0 layout0
  let x1 = ["Goal 1", "Goal 2", "Goal 3", "Goal 4", "Goal 5", "Goal 6", "Goal 7", "Goal 8", "Goal 9", "Goal 10"]
      y1 = ["Club A", "Club B", "Club C"]
      typ1 = "heatmap"
      td1 =
        [
          smartTraceData x1 y1 typ1
            # withZ
              [ [1,1,0,1,1,0,0,0,1,1]
              , [0,1,1,1,0,0,1,1,1,0]
              , [1,1,1,1,1,1,1,1,1,0]
              ]
            # withMarker (Marker.defaultMarker # Marker.withColor "#004165")
            # withMode "lines+markers"
            # withName "Heatmap Data"
            # withColorscale (ColorScale (Tuple 0.0 "white" : Tuple 1.0 "green" : Nil))
            # withShowscale false
        ]
      layout1 =
        L.defaultLayout
          # L.withTitle "Toastmasters DCP Goal Completion by Club"
          # L.withXAxis (defaultAxisLayout # AL.withTitle "DCP Goals")
          # L.withYAxis (defaultAxisLayout # AL.withTitle "Clubs")
  newPlot (DivId "heatmap") td1 layout1
