module Plotly.Font where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Font = Font
  { family :: String
  , size :: Int
  , color :: String
  }
derive newtype instance arbitraryFont :: Arbitrary Font
derive newtype instance decodeJsonFont :: DecodeJson Font
derive newtype instance encodeJsonFont :: EncodeJson Font
derive instance genericFont :: Generic Font _
derive newtype instance eqFont :: Eq Font
instance showFont :: Show Font where
  show = genericShow
