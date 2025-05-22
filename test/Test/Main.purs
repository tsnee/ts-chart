module Test.Main where

import Prelude

import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Test.QuickCheck (quickCheck)

import Plotly.Layout (Layout)
import Plotly.Marker (Marker)
import Plotly.TraceData (TraceData)

roundTripTestLayout :: Layout -> Boolean
roundTripTestLayout t = decodeJson (encodeJson t) == Right t

roundTripTestMarker :: Marker -> Boolean
roundTripTestMarker t = decodeJson (encodeJson t) == Right t

roundTripTestTraceData :: TraceData -> Boolean
roundTripTestTraceData t = decodeJson (encodeJson t) == Right t

main :: Effect Unit
main = do
  quickCheck roundTripTestLayout
  quickCheck roundTripTestMarker
  quickCheck roundTripTestTraceData

