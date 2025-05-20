module Plotly.Layout where

import Foreign (unsafeToForeign)
import ForeignEncoder (class ForeignEncoder)

newtype Layout = Layout { title :: String }
instance foreignEncoderLayout :: ForeignEncoder Layout where
  encode = unsafeToForeign
