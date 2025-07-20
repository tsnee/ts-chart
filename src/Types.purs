module Types
  ( AreaId(..)
  , ClubDataResponse(..)
  , ClubId(..)
  , ClubMetadataResponse(..)
  , Codomain(..)
  , DateChangedEvent(..)
  , DistrictId(..)
  , DivisionId(..)
  , Organization(..)
  , Series(..)
  , StartOrEnd(..)
  , iso8601Format
  , iso8601Parse
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Date (Date)
import Data.DateTime (DateTime(..), date)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

newtype ClubDataResponse = ClubDataResponse { club_number :: Int, series :: Array Series }

derive newtype instance decodeJsonClubDataResponse :: DecodeJson ClubDataResponse

newtype ClubMetadataResponse = ClubMetadataResponse
  { club_name :: String
  , club_number :: Int
  , area :: Int
  , division :: String
  , district :: Int
  }

derive newtype instance decodeJsonClubMetadataResponse :: DecodeJson ClubMetadataResponse

data DateChangedEvent = StartDateChanged Date | EndDateChanged Date

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

separator :: FormatterCommand
separator = Placeholder "-"

iso8601Date :: Formatter
iso8601Date = YearFull : separator : MonthTwoDigits : separator : DayOfMonthTwoDigits : Nil

iso8601Format :: Date -> String
iso8601Format date = format iso8601Date dateTime
  where
  dateTime = DateTime date bottom

iso8601Parse :: String -> Maybe Date
iso8601Parse s = date <$> (hush $ unformat iso8601Date s)
