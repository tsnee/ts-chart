module Plotly.Shape (Shape(..), showLower) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (toLower)

data Shape = Linear | Spline | Hv | Hvh | Vh | Vhv
derive instance genericShape :: Generic Shape _
instance showShape :: Show Shape where
    show = genericShow

showLower :: Shape -> String
showLower = toLower <<< show
