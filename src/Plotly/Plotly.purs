module Plotly.Plotly (newPlot) where

import Prelude

import Effect (Effect)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Function.Uncurried (Fn3, runFn3)

import Plotly.DivId (DivId)
import Plotly.Layout (Layout)
import Plotly.TraceData (TraceData)

newPlot :: DivId -> Array TraceData -> Layout -> Effect Unit
newPlot divId dataArray layout = runFn3 _newPlot
  (encodeJson divId)
  (encodeJson dataArray)
  (encodeJson layout)

foreign import _newPlot :: Fn3 Json Json Json (Effect Unit)
