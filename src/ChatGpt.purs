module ChatGpt where

import Prelude

import Data.Array (map)
import Data.Time (DateTime)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Data.Maybe (Maybe(..))
import Plotly (plot)
import Plotly.Types as PT

-- Dummy data: replace with real time series
exampleDates :: Array String
exampleDates = [
  "2025-01-01",
  "2025-02-01",
  "2025-03-01"
]

exampleValues :: Array Number
exampleValues = [10.0, 15.0, 20.0]

-- Convert the data into a Plotly trace
exampleTrace :: PT.Trace
exampleTrace = PT.lineTrace
  { x: PT.Strs exampleDates
  , y: PT.Nums exampleValues
  , name: Just "Example Time Series"
  }

-- Plot layout (optional)
layout :: PT.Layout
layout = PT.defaultLayout
  { title: Just "Example Time Series Chart"
  }

main :: Effect Unit
main = do
  log "Rendering chart..."
  plot "chart" [exampleTrace] layout
