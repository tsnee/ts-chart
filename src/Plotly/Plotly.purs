module Plotly.Plotly (newPlot, updatePlot) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Options, options)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Web.HTML.HTMLElement (HTMLElement)

import Plotly.DivId (DivId)
import Plotly.Layout (Layout)
import Plotly.TraceData (TraceData)

newPlot :: DivId -> Array (Options TraceData) -> Options Layout -> Effect HTMLElement
newPlot divId traceData layout = runFn3 _newPlot
  (unsafeToForeign divId)
  (options <$> traceData)
  (options layout)

updatePlot :: HTMLElement -> TraceData -> Layout -> Effect Unit
updatePlot plot traceData layout = runFn3 _updatePlot plot traceData layout

foreign import _newPlot :: Fn3 Foreign (Array Foreign) Foreign (Effect HTMLElement)

foreign import _updatePlot :: Fn3 HTMLElement TraceData Layout (Effect Unit)
