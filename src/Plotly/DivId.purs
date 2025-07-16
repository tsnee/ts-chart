module Plotly.DivId (DivId(..)) where

import Prelude

import Data.Newtype (class Newtype)

newtype DivId = DivId String
derive newtype instance eqDivId :: Eq DivId
derive instance newtypeDivId :: Newtype DivId _
