module Plotly.TraceData where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Test.QuickCheck.Arbitrary (class Arbitrary)

import Plotly.Line (Line)
import Plotly.Marker (Marker)

newtype TraceData = TraceData
  { x :: Array Int
  , y :: Array Int
  , line :: Maybe Line
  , marker :: Marker
  , mode :: String
  , type :: String
  }
derive newtype instance arbitraryTraceData :: Arbitrary TraceData
derive newtype instance decodeJsonTraceData :: DecodeJson TraceData
derive newtype instance encodeJsonTraceData :: EncodeJson TraceData
derive newtype instance eqTraceData :: Eq TraceData
