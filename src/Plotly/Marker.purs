module Plotly.Marker
( Marker(..)
, defaultMarker
, withColor
, withSize
) where

import Prelude

import Control.Monad.Gen.Common (genMaybe)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), optional)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements)
import Data.NonEmpty (NonEmpty(..))

newtype Marker = Marker
  { color :: Maybe String
  , size  :: Maybe Int
  }
derive newtype instance eqMarker :: Eq Marker
derive newtype instance decodeJsonMarker :: DecodeJson Marker
derive newtype instance encodeJsonMarker :: EncodeJson Marker

instance arbitraryMarker :: Arbitrary Marker where
  arbitrary = do
    color <- optional $ elements $ fromNonEmpty $ NonEmpty "red" ["blue", "green"]
    size <- optional $ chooseInt 1 72
    pure $ Marker { color, size }

defaultMarker :: Marker
defaultMarker = Marker { color: Nothing, size: Nothing }

withColor :: String -> Marker -> Marker
withColor c (Marker m) = Marker $ m { color = Just c }

withSize :: Int -> Marker -> Marker
withSize n (Marker m) = Marker $ m { size = Just n }
