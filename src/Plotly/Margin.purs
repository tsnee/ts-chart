module Plotly.Margin where

import Prelude

import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Margin = Margin
  { l :: Int
  , r :: Int
  , t :: Int
  , b :: Int
  }
derive newtype instance arbitraryMargin :: Arbitrary Margin
derive newtype instance decodeJsonMargin :: DecodeJson Margin
derive newtype instance encodeJsonMargin :: EncodeJson Margin
derive instance genericMargin :: Generic Margin _
derive newtype instance eqMargin :: Eq Margin
instance showMargin :: Show Margin where
  show = genericShow
