module Plotly.Margin (Margin(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Margin = Margin
  { l :: Int
  , r :: Int
  , t :: Int
  , b :: Int
  }
derive instance genericMargin :: Generic Margin _
derive newtype instance eqMargin :: Eq Margin
instance showMargin :: Show Margin where
  show = genericShow
