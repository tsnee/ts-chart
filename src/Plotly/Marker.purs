module Plotly.Marker where

import Prelude
import Data.Options (Option, opt)

data Marker

color :: Option Marker String
color = opt "color"

size :: Option Marker Int
size = opt "size"
