module Plotly.TraceData where

import Plotly.Marker (Marker)
import Foreign (unsafeToForeign)
import ForeignEncoder (class ForeignEncoder)

newtype TraceData = TraceData
  { x :: Array Int
  , y :: Array Int
  , type :: String
  , mode :: String
  , marker :: Marker
  }
instance foreignEncoderTraceData :: ForeignEncoder TraceData where
  encode = unsafeToForeign
