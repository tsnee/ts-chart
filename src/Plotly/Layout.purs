module Plotly.Layout where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

import Plotly.AxisLayout (AxisLayout)

newtype Layout = Layout
  { title :: Maybe String
  , xaxis :: Maybe AxisLayout
  , yaxis :: Maybe AxisLayout
  }
derive newtype instance arbitraryLayout :: Arbitrary Layout
derive newtype instance decodeJsonLayout :: DecodeJson Layout
derive newtype instance encodeJsonLayout :: EncodeJson Layout
derive instance genericLayout :: Generic Layout _
derive newtype instance eqLayout :: Eq Layout
instance showLayout :: Show Layout where
  show = genericShow

defaultLayout :: Layout
defaultLayout = Layout { title: Nothing, xaxis: Nothing, yaxis: Nothing }

withTitle :: String -> Layout -> Layout
withTitle t (Layout l) = Layout $ l { title = Just t }
