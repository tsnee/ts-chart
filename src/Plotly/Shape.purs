module Plotly.Shape where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (Foreign, unsafeToForeign)

data Shape = Linear | Spline | Hv | Hvh | Vh | Vhv
derive instance genericShape :: Generic Shape _
instance showShape :: Show Shape where
    show = genericShow

toForeignStringLower :: Shape -> Foreign
toForeignStringLower Linear = unsafeToForeign "linear"
toForeignStringLower Spline = unsafeToForeign "spline"
toForeignStringLower Hv = unsafeToForeign "hv"
toForeignStringLower Hvh = unsafeToForeign "hvh"
toForeignStringLower Vh = unsafeToForeign "vh"
toForeignStringLower Vhv = unsafeToForeign "vhv"
