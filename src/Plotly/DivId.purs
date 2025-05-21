module Plotly.DivId where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Eq (class Eq)

newtype DivId = DivId String
derive newtype instance eqDivId :: Eq DivId
derive newtype instance decodeJsonDivId :: DecodeJson DivId
derive newtype instance encodeJsonDivId :: EncodeJson DivId
