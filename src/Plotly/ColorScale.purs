module Plotly.ColorScale where

import Prelude

import Data.Argonaut.Core (fromNumber, fromString, fromArray)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.List (toUnfoldable)
import Data.List.Types (List)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype ColorScale = ColorScale (Array (Tuple Number String))
derive newtype instance arbitraryColorScale :: Arbitrary ColorScale
derive newtype instance decodeJsonColorScale :: DecodeJson ColorScale
instance encodeJsonColorScale :: EncodeJson ColorScale where
  encodeJson (ColorScale x) = fromArray $ do
    Tuple n s <- x
    pure $ fromArray [fromNumber n, fromString s]
derive instance genericColorScale :: Generic ColorScale _
derive newtype instance eqColorScale :: Eq ColorScale
instance showColorScale :: Show ColorScale where
  show = genericShow