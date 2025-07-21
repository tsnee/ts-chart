module Plotly.Plotly (newPlot, updatePlot) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Options, options)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Web.DOM.Element (Element, toNode)
import Web.DOM.Node (Node)

import Plotly.DivId (DivId)
import Plotly.Layout (Layout)
import Plotly.TraceData (TraceData)

newPlot :: DivId -> Array (Options TraceData) -> Options Layout -> Effect Element
newPlot divId traceData layout = runFn3 _newPlot
  (unsafeToForeign divId)
  (options <$> traceData)
  (options layout)

updatePlot :: Element -> Array (Options TraceData) -> Options Layout -> Effect Unit
updatePlot plot traceData layout = runFn3 _updatePlot
  (toNode plot)
  (options <$> traceData)
  (options layout)

foreign import _newPlot :: Fn3 Foreign (Array Foreign) Foreign (Effect Element)

foreign import _updatePlot :: Fn3 Node (Array Foreign) Foreign (Effect Unit)
