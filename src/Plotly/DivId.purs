module Plotly.DivId where

import Prelude

newtype DivId = DivId String
derive newtype instance eqDivId :: Eq DivId
