module Plotly.Layout
( Layout
, autosize
, barmode
, font
, legend
, margin
, showlegend
, title
, xaxis
, yaxis
) where

import Prelude

import Data.Functor.Contravariant (cmap)
import Data.Options (Option, Options, opt, options)

import Plotly.AxisLayout (AxisLayout)
import Plotly.Font (Font)
import Plotly.Legend (Legend)
import Plotly.Margin (Margin)

data Layout

autosize :: Option Layout Boolean
autosize = opt "autosize"

barmode :: Option Layout String
barmode = opt "barmode"

font :: Option Layout Font
font = opt "font"

legend :: Option Layout Legend
legend = opt "legend"

margin :: Option Layout Margin
margin = opt "margin"

showlegend :: Option Layout Boolean
showlegend = opt "showlegend"

title :: Option Layout String
title = opt "title"

xaxis :: Option Layout (Options AxisLayout)
xaxis = cmap options $ opt "xaxis"

yaxis :: Option Layout (Options AxisLayout)
yaxis = cmap options $ opt "yaxis"
