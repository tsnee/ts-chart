module Plotly.Legend where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Legend = Legend
  { orientation :: String
  , x :: Number
  , y :: Number
  }
derive newtype instance arbitraryLegend :: Arbitrary Legend
derive newtype instance decodeJsonLegend :: DecodeJson Legend
derive newtype instance encodeJsonLegend :: EncodeJson Legend
derive instance genericLegend :: Generic Legend _
derive newtype instance eqLegend :: Eq Legend
instance showLegend :: Show Legend where
  show = genericShow
