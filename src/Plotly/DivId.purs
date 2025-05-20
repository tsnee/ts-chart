module Plotly.DivId where

import Foreign (unsafeToForeign)
import ForeignEncoder (class ForeignEncoder)

newtype DivId = DivId String
instance foreignEncoderDivId :: ForeignEncoder DivId where
  encode = unsafeToForeign
