module Plotly.Line where

import Prelude

import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Decode.Error (JsonDecodeError(UnexpectedValue))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (oneOf)

data Shape = Linear | Spline | Hv | Hvh | Vh | Vhv
instance arbitraryShape :: Arbitrary Shape where
  arbitrary = oneOf $ pure <$> NEA.cons' Linear [ Spline, Hv, Hvh, Vh, Vhv ]
instance encodeJsonShape :: EncodeJson Shape where
  encodeJson = fromString <<< toLower <<< show
instance decodeJsonShape :: DecodeJson Shape where
  decodeJson json = do
    s <- decodeString json
    case toLower s of
      "linear" -> pure Linear
      "spline" -> pure Spline
      "hv" -> pure Hv
      "hvh" -> pure Hvh
      "vh" -> pure Vh
      "vhv" -> pure Vhv
      _        -> Left $ UnexpectedValue json
derive instance eqShape :: Eq Shape
derive instance genericShape :: Generic Shape _
instance showShape :: Show Shape where
  show = genericShow

newtype Line = Line
  { cmin :: Maybe Number
  , cmax :: Maybe Number
  , color :: Maybe String
  , coloraxis :: Maybe String
  , colorscale :: Maybe (Array String)
  , dash :: Maybe String
  , reversescale :: Maybe Boolean
  , shape :: Maybe Shape
  , simplify :: Maybe Boolean
  , smoothing :: Maybe Number
  , width :: Maybe Int
  }
derive newtype instance arbitraryLine :: Arbitrary Line
derive newtype instance decodeJsonLine :: DecodeJson Line
derive newtype instance encodeJsonLine :: EncodeJson Line
derive instance genericLine :: Generic Line _
derive newtype instance eqLine :: Eq Line
instance showLine :: Show Line where
  show = genericShow

defaultLine :: Line
defaultLine = Line
         { cmin: Nothing
         , cmax: Nothing
         , color: Nothing
         , coloraxis: Nothing
         , colorscale: Nothing
         , dash: Nothing
         , reversescale: Nothing
         , shape: Nothing
         , simplify: Nothing
         , smoothing: Nothing
         , width: Nothing
         }

withCMinMax :: Number -> Number -> Line -> Line
withCMinMax min max (Line l) = Line $ l { cmin = Just min, cmax = Just max }

withColor :: String -> Line -> Line
withColor s (Line l) = Line $ l { color = Just s }

withShape :: Shape -> Line -> Line
withShape s (Line l) = Line $ l { shape = Just s }
