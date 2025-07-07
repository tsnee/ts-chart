module Utils (Codomain(..), Response(..), Series(..)) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))


newtype Response = Response { club_number :: Int, series :: Array Series }
derive newtype instance decodeJsonResponse :: DecodeJson Response

newtype Series = Series { label :: String, domain :: Array String, codomain :: Codomain }
derive newtype instance decodeJsonSeries :: DecodeJson Series

data Codomain = IntCodomain (Array Int) | StringCodomain (Array String)
instance decodeJsonCodomain :: DecodeJson Codomain where
  decodeJson json =
    case decodeJson json :: Either JsonDecodeError (Array Int) of
      Right ints -> Right (IntCodomain ints)
      Left _ ->
        case decodeJson json :: Either JsonDecodeError (Array String) of
          Right strs -> Right (StringCodomain strs)
          Left _ -> Left $ TypeMismatch "Codomain: expected array of ints or strings"
