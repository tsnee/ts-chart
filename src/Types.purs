module Types
  ( AreaId(..)
  , ClubId(..)
  , Codomain(..)
  , DistrictId(..)
  , DivisionId(..)
  , Organization(..)
  , Response(..)
  , Series(..)
  , StartOrEnd(..)
  , iso8601Format
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Date (Date, day, month, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Utils (padStart)

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

data Organization
  = Club ClubId
  | Area AreaId
  | Division DivisionId
  | District DistrictId

derive instance genericOrganization :: Generic Organization _
instance showOrganization :: Show Organization where
  show = genericShow

newtype ClubId = ClubId Int

derive newtype instance showClubId :: Show ClubId

data AreaId = AreaId Int | AreaNotAssigned

derive instance genericAreaId :: Generic AreaId _
instance showAreaId :: Show AreaId where
  show = genericShow

data DivisionId = DivisionId Char | DivisionNotAssigned

derive instance genericDivisionId :: Generic DivisionId _
instance showDivisionId :: Show DivisionId where
  show = genericShow

newtype DistrictId = DistrictId Int

derive newtype instance showDistrictId :: Show DistrictId

data StartOrEnd = Start | End

instance showDateType :: Show StartOrEnd where
  show Start = "Start"
  show End = "End"

iso8601Format :: Date -> String
iso8601Format date =
  let
    y = show $ fromEnum $ year date
    m = pad2 $ fromEnum $ month date
    d = pad2 $ fromEnum $ day date
    pad2 i = replaceAll (Pattern " ") (Replacement "0") $ padStart 2 $ show $ clamp 0 31 i
  in
    joinWith "-" [ y, m, d ]
