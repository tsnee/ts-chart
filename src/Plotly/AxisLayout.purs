module Plotly.AxisLayout where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

import SparseEncoder (field, sparseRecordToJson)

newtype AxisLayout = AxisLayout
  { gridcolor :: Maybe String
  , showgrid :: Maybe Boolean
  , title :: Maybe String
  , zeroline :: Maybe Boolean
  }
derive newtype instance arbitraryAxisLayout :: Arbitrary AxisLayout
derive newtype instance decodeJsonAxisLayout :: DecodeJson AxisLayout
instance encodeJsonAxisLayout :: EncodeJson AxisLayout where
  encodeJson (AxisLayout x) = sparseRecordToJson
    [ field "gridcolor" x.gridcolor
    , field "showgrid" x.showgrid
    , field "title" x.title
    , field "zeroline" x.zeroline
    ]
derive instance genericAxisLayout :: Generic AxisLayout _
derive newtype instance eqAxisLayout :: Eq AxisLayout
instance showAxisLayout :: Show AxisLayout where
  show = genericShow

defaultAxisLayout :: AxisLayout
defaultAxisLayout = AxisLayout
  { gridcolor: Nothing
  , showgrid: Nothing
  , title: Nothing
  , zeroline: Nothing
  }

withGridcolor :: String -> AxisLayout -> AxisLayout
withGridcolor c (AxisLayout l) = AxisLayout $ l { gridcolor = Just c, showgrid = Just true }

withShowgrid :: Boolean -> AxisLayout -> AxisLayout
withShowgrid b (AxisLayout l) = AxisLayout $ l { showgrid = Just b }

withTitle :: String -> AxisLayout -> AxisLayout
withTitle t (AxisLayout l) = AxisLayout $ l { title = Just t }

withZeroline :: Boolean -> AxisLayout -> AxisLayout
withZeroline b (AxisLayout l) = AxisLayout $ l { zeroline = Just b }
