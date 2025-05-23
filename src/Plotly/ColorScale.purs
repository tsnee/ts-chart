module Plotly.ColorScale (ColorScale(..), toForeign) where

import Prelude

import Data.Tuple (Tuple(..))
import Foreign (Foreign, unsafeToForeign)

newtype ColorScale = ColorScale (Array (Tuple Number String))

toForeign :: ColorScale -> Array (Array Foreign)
toForeign (ColorScale x) = do
    Tuple n s <- x
    pure [unsafeToForeign n, unsafeToForeign s]
