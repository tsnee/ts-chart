module Plotly.Line where

import Prelude

import Data.Functor.Contravariant (cmap)
import Data.Options (Option, opt)

import Plotly.Shape (Shape, toForeignStringLower)

data Line

cmin :: Option Line Number
cmin = opt "cmin"

cmax :: Option Line Number
cmax = opt "cmax"

color :: Option Line String
color = opt "color"

coloraxis :: Option Line String
coloraxis = opt "coloraxis"

colorscale :: Option Line (Array String)
colorscale = opt "colorscale"

dash :: Option Line String
dash = opt "dash"

reversescale :: Option Line Boolean
reversescale = opt "reversescale"

shape :: Option Line Shape
shape = cmap toForeignStringLower (opt "shape")

simplify :: Option Line Boolean
simplify = opt "simplify"

smoothing :: Option Line Number
smoothing = opt "smoothing"

width :: Option Line Int
width = opt "width"
