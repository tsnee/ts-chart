module Plotly.Legend (Legend(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Legend = Legend
  { orientation :: String
  , x :: Number
  , y :: Number
  }
derive instance genericLegend :: Generic Legend _
derive newtype instance eqLegend :: Eq Legend
instance showLegend :: Show Legend where
  show = genericShow
