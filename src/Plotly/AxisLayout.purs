module Plotly.AxisLayout
  ( AxisLayout
  , gridcolor
  , showgrid
  , title
  , zeroline
  ) where

import Data.Options (Option, opt)

data AxisLayout

gridcolor :: Option AxisLayout String
gridcolor = opt "gridcolor"

showgrid :: Option AxisLayout Boolean
showgrid = opt "showgrid"

title :: Option AxisLayout String
title = opt "title"

zeroline :: Option AxisLayout Boolean
zeroline = opt "zeroline"
