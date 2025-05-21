module Plotly.Layout where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Maybe (Maybe)

newtype Layout = Layout { title :: Maybe String }
derive newtype instance eqLayout :: Eq Layout
derive newtype instance decodeJsonLayout :: DecodeJson Layout
derive newtype instance encodeJsonLayout :: EncodeJson Layout
