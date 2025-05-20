module Plotly.Plotly (newPlot) where

import Prelude

import Effect (Effect)
import Foreign (Foreign)
import Data.Function.Uncurried (Fn3, runFn3)
import ForeignEncoder (encode)

import Plotly.TraceData (TraceData)
import Plotly.DivId (DivId)
import Plotly.Layout (Layout)

newPlot :: DivId -> Array TraceData -> Layout -> Effect Unit
newPlot divId dataArray layout = runFn3 _newPlot
  (encode divId)
  (encode dataArray)
  (encode layout)

foreign import _newPlot :: Fn3 Foreign Foreign Foreign (Effect Unit)
