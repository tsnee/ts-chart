module Plotly.Line where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Line = Line { shape :: String }
derive newtype instance arbitraryLine :: Arbitrary Line
derive newtype instance decodeJsonLine :: DecodeJson Line
derive newtype instance encodeJsonLine :: EncodeJson Line
derive instance genericLine :: Generic Line _
derive newtype instance eqLine :: Eq Line
instance showLine :: Show Line where
  show = genericShow
