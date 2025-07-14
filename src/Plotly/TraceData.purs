module Plotly.TraceData
  ( TraceData
  , x
  , xaxis
  , y
  , yaxis
  , z
  , zaxis
  , colorscale
  , fill
  , fillcolor
  , hovertext
  , legendgroup
  , line
  , marker
  , mode
  , name
  , opacity
  , showlegend
  , showscale
  , stackgroup
  , text
  , typ
  ) where

import Prelude

import Data.Functor.Contravariant (cmap)
import Data.Options (Option, Options, opt, options)

import Plotly.ColorScale (ColorScale, toForeign)
import Plotly.Line (Line)
import Plotly.Marker (Marker)

data TraceData

x :: forall a. Option TraceData (Array a)
x = opt "x"

xaxis :: Option TraceData String
xaxis = opt "xaxis"

y :: forall b. Option TraceData (Array b)
y = opt "y"

yaxis :: Option TraceData String
yaxis = opt "yaxis"

z :: forall c. Option TraceData (Array c)
z = opt "z"

zaxis :: Option TraceData String
zaxis = opt "zaxis"

colorscale :: Option TraceData ColorScale
colorscale = cmap toForeign $ opt "colorscale"

fill :: Option TraceData String
fill = opt "fill"

fillcolor :: Option TraceData String
fillcolor = opt "fillcolor"

hovertext :: Option TraceData (Array String)
hovertext = opt "hovertext"

legendgroup :: Option TraceData String
legendgroup = opt "legendgroup"

line :: Option TraceData (Options Line)
line = cmap options $ opt "line"

marker :: Option TraceData (Options Marker)
marker = cmap options $ opt "marker"

mode :: Option TraceData String
mode = opt "mode"

name :: Option TraceData String
name = opt "name"

opacity :: Option TraceData Number
opacity = opt "opacity"

showlegend :: Option TraceData Boolean
showlegend = opt "showlegend"

showscale :: Option TraceData Boolean
showscale = opt "showscale"

stackgroup :: Option TraceData String
stackgroup = opt "stackgroup"

text :: Option TraceData (Array String)
text = opt "text"

typ :: Option TraceData String
typ = opt "type"
