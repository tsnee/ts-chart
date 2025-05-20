module ForeignEncoder where

import Foreign (Foreign, unsafeToForeign)

class ForeignEncoder a where
  encode :: a -> Foreign
instance foreignEncoderArray :: ForeignEncoder a => ForeignEncoder (Array a) where
  encode = unsafeToForeign
