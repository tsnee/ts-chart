module Demo where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(POST))
import Data.Options (Options, (:=))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Effect.Exception (Error, message)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)

import Plotly.AxisLayout (gridcolor, showgrid, title, zeroline) as AL
import Plotly.ColorScale (ColorScale(..))
import Plotly.DivId (DivId(..))
import Plotly.Font (Font(..))
import Plotly.Layout (autosize, font, legend, margin, showlegend, title, xaxis, yaxis)
import Plotly.Legend (Legend(..))
import Plotly.Line (color, shape) as Line
import Plotly.Margin (Margin(..))
import Plotly.Marker (color) as Marker
import Plotly.Plotly (newPlot)
import Plotly.Shape (Shape(..))
import Plotly.TraceData (TraceData, colorscale, fill, fillcolor, line, marker, mode, name, showscale, typ, x, y, z)
import Plotly.Line (shape)
import Types (Codomain(..), Response(..), Series(..))

fetchaff :: Aff Response
fetchaff = do
  { json } <- fetch "https://api.brightringsofteare.com/measurements/club"
    { method: POST
    , body: toJsonString {"club_number":2490993,"metrics":["MembershipBase","NewMembers","ActiveMembers"]}
    , headers: { "Content-Type": "application/json" }
    }
  x :: Response <- fromJson json
  pure x

stackedLineChart :: Response -> Effect Unit
stackedLineChart (Response{series}) = do
  let layout = title := "Stacked Area Chart"
      td :: Array (Options TraceData)
      td = do
        Series{ label, domain, codomain } <- series
        pure $
          x := domain
          <> case codomain of
            IntCodomain ys -> y := ys
            StringCodomain ys -> y := ys
          <> name := label
          <> typ := "scatter"
          <> line := (shape := Vh)
          <> mode := "lines"
  void $ newPlot (DivId "stacked") td layout

lineChart :: Effect Unit
lineChart = do
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
  void $ newPlot (DivId "linechart") td0 layout0

heatmap :: Effect Unit
heatmap = do
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
        <> colorscale := (ColorScale [Tuple 0.0 "#A9B2B1", Tuple 1.0 "#772432"])
        <> showscale := false
        ]
      layout1 =
        title := "Toastmasters DCP Goal Completion by Club"
        <> xaxis := (AL.title := "DCP Goals")
        <> yaxis := (AL.title := "Clubs")
  void $ newPlot (DivId "heatmap") td1 layout1

logFetchResult :: Either Error Response -> Effect Unit
logFetchResult (Left err) = log $ message err
logFetchResult (Right a)  = stackedLineChart a

main :: Effect Unit
main = do
  runAff_ logFetchResult fetchaff
  heatmap
  lineChart
