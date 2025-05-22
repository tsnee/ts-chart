module Plotly.AxisLayout where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype AxisLayout = AxisLayout { title :: Maybe String }
derive newtype instance arbitraryAxisLayout :: Arbitrary AxisLayout
derive newtype instance decodeJsonAxisLayout :: DecodeJson AxisLayout
derive newtype instance encodeJsonAxisLayout :: EncodeJson AxisLayout
derive instance genericAxisLayout :: Generic AxisLayout _
derive newtype instance eqAxisLayout :: Eq AxisLayout
instance showAxisLayout :: Show AxisLayout where
  show = genericShow

defaultAxisLayout :: AxisLayout
defaultAxisLayout = AxisLayout { title: Nothing }

withTitle :: String -> AxisLayout -> AxisLayout
withTitle t (AxisLayout l) = AxisLayout $ l { title = Just t }
