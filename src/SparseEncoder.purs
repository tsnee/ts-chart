module SparseEncoder (field, sparseRecordToJson) where

import Prelude

import Data.Argonaut.Core (Json, fromObject)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Array (mapMaybe)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (fromFoldable) as FO

type Field = Maybe (Tuple String Json)

-- | Build an optional (key , encoded-value) pair.
field ::
  forall a.
  EncodeJson a =>      -- any encodable value
  String ->            -- JSON key
  Maybe a ->           -- Maybe value
  Field
field k = map \v -> Tuple k (encodeJson v)

sparseRecordToJson :: Array Field -> Json
sparseRecordToJson = fromObject <<< FO.fromFoldable <<< mapMaybe identity
