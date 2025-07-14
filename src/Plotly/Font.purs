module Plotly.Font (Font(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Font = Font
  { family :: String
  , size :: Int
  , color :: String
  }

derive instance genericFont :: Generic Font _
derive newtype instance eqFont :: Eq Font
instance showFont :: Show Font where
  show = genericShow
