module Plotly.Layout where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

import Plotly.AxisLayout (AxisLayout)
import Plotly.Font (Font)
import Plotly.Legend (Legend)
import Plotly.Margin (Margin)
import SparseEncoder (field, sparseRecordToJson)

newtype Layout = Layout
  { autosize :: Maybe Boolean
  , barmode :: Maybe String
  , font :: Maybe Font
  , legend :: Maybe Legend
  , margin :: Maybe Margin
  , showlegend :: Maybe Boolean
  , title :: Maybe String
  , xaxis :: Maybe AxisLayout
  , yaxis :: Maybe AxisLayout
  }
derive newtype instance arbitraryLayout :: Arbitrary Layout
derive newtype instance decodeJsonLayout :: DecodeJson Layout
instance encodeJsonLayout :: EncodeJson Layout where
  encodeJson (Layout x) = sparseRecordToJson
    [ field "autosize" x.autosize
    , field "barmode" x.barmode
    , field "font" x.font
    , field "legend" x.legend
    , field "margin" x.margin
    , field "showlegend" x.showlegend
    , field "title" x.title
    , field "xaxis" x.xaxis
    , field "yaxis" x.yaxis
    ]
derive instance genericLayout :: Generic Layout _
derive newtype instance eqLayout :: Eq Layout
instance showLayout :: Show Layout where
  show = genericShow

defaultLayout :: Layout
defaultLayout = Layout
  { autosize: Nothing
  , barmode: Nothing
  , font: Nothing
  , legend: Nothing
  , margin: Nothing
  , showlegend: Nothing
  , title: Nothing
  , xaxis: Nothing
  , yaxis: Nothing
  }

withAutosize :: Boolean -> Layout -> Layout
withAutosize b (Layout l) = Layout $ l { autosize = Just b }

withBarmode :: String -> Layout -> Layout
withBarmode s (Layout l) = Layout $ l { barmode = Just s }

withFont :: Font -> Layout -> Layout
withFont f (Layout l) = Layout $ l { font = Just f }

withShowlegend :: Boolean -> Layout -> Layout
withShowlegend b (Layout l) = Layout $ l { showlegend = Just b }

withTitle :: String -> Layout -> Layout
withTitle t (Layout l) = Layout $ l { title = Just t }

withXAxis :: AxisLayout -> Layout -> Layout
withXAxis al (Layout l) = Layout $ l { xaxis = Just al }

withYAxis :: AxisLayout -> Layout -> Layout
withYAxis al (Layout l) = Layout $ l { yaxis = Just al }
