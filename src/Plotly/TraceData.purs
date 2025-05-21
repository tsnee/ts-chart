module Plotly.TraceData where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Eq (class Eq)

import Plotly.Marker (Marker)

newtype TraceData = TraceData
  { x :: Array Int
  , y :: Array Int
  , type :: String
  , mode :: String
  , marker :: Marker
  }
derive newtype instance eqTraceData :: Eq TraceData
derive newtype instance decodeJsonTraceData :: DecodeJson TraceData
derive newtype instance encodeJsonTraceData :: EncodeJson TraceData
