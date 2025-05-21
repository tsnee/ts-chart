module Test.Main where

import Prelude

import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Test.QuickCheck (quickCheck)

import Plotly.Marker (Marker)

roundTripTest :: Marker -> Boolean
roundTripTest t = decodeJson (encodeJson t) == Right t

main :: Effect Unit
main = quickCheck roundTripTest

