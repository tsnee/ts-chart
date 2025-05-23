module Main where

import Prelude

import Data.Options ((:=))
import Data.Tuple (Tuple(..))
import Effect (Effect)

import Plotly.AxisLayout (gridcolor, showgrid, title, zeroline) as AL
import Plotly.ColorScale (ColorScale(..))
import Plotly.DivId (DivId(..))
import Plotly.Font (Font(..))
import Plotly.Layout (autosize, font, legend, margin, showlegend, title, xaxis, yaxis)
import Plotly.Legend (Legend(Legend))
import Plotly.Line (color, shape) as Line
import Plotly.Margin (Margin(..))
import Plotly.Marker (color) as Marker
import Plotly.Plotly (newPlot)
import Plotly.Shape (Shape(..))
import Plotly.TraceData (colorscale, fill, fillcolor, line, marker, mode, name, showscale, typ, x, y, z)

main :: Effect Unit
main = do
  let td0 =
        [ x := [1, 2, 3, 4]
        <> y := [10, 15, 13, 17]
        <> typ := "scatter"
        <> name := "Line Chart Data"
        <> line := (Line.color := "#004165" <> Line.shape := Linear)
        <> marker := (Marker.color := "#004165")
        <> mode := "lines+markers"
        <> fill := "tozeroy"
        <> fillcolor := "#004165"
        ]
      layout0 =
        autosize := true
        <> font := Font { family: "Source Sans 3, sans-serif", size: 16, color: "#772432" }
        <> legend := Legend { orientation: "h", x: 1.0, y: 1.1 }
        <> margin := Margin { l: 40, r: 40, t: 60, b: 40 }
        <> showlegend := true
        <> title := "Line Chart Title"
        <> xaxis := (AL.title := "Time" <> AL.showgrid := false <> AL.zeroline := false)
        <> yaxis := (AL.title := "DCP Points" <> AL.gridcolor := "#eee")
  newPlot (DivId "linechart") td0 layout0
  let td1 =
        [ x := ["Goal 1", "Goal 2", "Goal 3", "Goal 4", "Goal 5", "Goal 6", "Goal 7", "Goal 8", "Goal 9", "Goal 10"]
        <> y := ["Club A", "Club B", "Club C"]
        <> typ := "heatmap"
        <> z :=
              [ [1,1,0,1,1,0,0,0,1,1]
              , [0,1,1,1,0,0,1,1,1,0]
              , [1,1,1,1,1,1,1,1,1,0]
              ]
        <> name := "Heatmap Data"
        <> colorscale := (ColorScale [Tuple 0.0 "white", Tuple 1.0 "green"])
        <> showscale := false
        ]
      layout1 =
        title := "Toastmasters DCP Goal Completion by Club"
        <> xaxis := (AL.title := "DCP Goals")
        <> yaxis := (AL.title := "Clubs")
  newPlot (DivId "heatmap") td1 layout1
