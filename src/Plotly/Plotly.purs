module Plotly.Plotly (newPlot) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Options, options)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)

import Plotly.DivId (DivId)
import Plotly.Layout (Layout)
import Plotly.TraceData (TraceData)

newPlot :: DivId -> Array (Options TraceData) -> Options Layout -> Effect Unit
newPlot divId traceData layout = runFn3 _newPlot
  (unsafeToForeign divId)
  (options <$> traceData)
  (options layout)

foreign import _newPlot :: Fn3 Foreign (Array Foreign) Foreign (Effect Unit)
