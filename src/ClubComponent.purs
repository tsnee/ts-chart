module ClubComponent (Output, Query, component) where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Argonaut.Encode (toJsonString)
import Data.Const (Const)
import Data.Date (Date)
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Options (Options, (:=))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML.Common (ClassName(..))
import Web.HTML.HTMLElement (HTMLElement)

import Plotly.DivId (DivId(..))
import Plotly.Layout (title)
import Plotly.Plotly (newPlot, updatePlot)
import Plotly.Shape (Shape(..))
import Plotly.TraceData (TraceData, line, mode, name, typ, x, y)
import Plotly.Line (shape)
import Types (ClubId(..), Codomain(..), Response(..), Series(..), iso8601Format)

type State =
  { clubName :: String
  , clubNumber :: ClubId
  , metrics :: Array String
  , startDate :: Date
  , endDate :: Date
  , plot :: Maybe HTMLElement
  }

type Input = State

type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type Slots :: forall k. Row k
type Slots = ()

data Action
  = Initialize
  | ChangeMetrics (Array String)
  | ChangeStartDate Date
  | ChangeEndDate Date

chartDivId :: ClubId -> DivId
chartDivId (ClubId clubNumber) = DivId $ "club-" <> show clubNumber

component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState = identity
  render { clubName, clubNumber } = HH.div_
    [ HH.h3_ [ HH.text clubName ]
    , HH.div
        [ HP.id $ unwrap $ chartDivId clubNumber
        , HP.class_ (ClassName "chart")
        ]
        []
    ]
  eval =
    H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Initialize -> draw
    ChangeMetrics metrics -> do
      log $ "changing metrics to " <> show metrics
      H.modify_ \s -> s { metrics = metrics }
      draw
    ChangeStartDate startDate -> do
      log $ "changing start date to " <> show startDate
      H.modify_ \s -> s { startDate = startDate }
      draw
    ChangeEndDate endDate -> do
      log $ "changing end date to " <> show endDate
      H.modify_ \s -> s { endDate = endDate }
      draw

fetchClubData :: forall m. MonadAff m => ClubId -> Array String -> Date -> Date -> m Response
fetchClubData (ClubId clubNumber) metrics startDate endDate = liftAff $ do
  { json } <- fetch "https://api.brightringsoftware.com/measurements/club"
    { method: POST
    , headers: { "Content-Type": "application/json" }
    , body: toJsonString
        { "club_number": clubNumber
        , "metrics": metrics
        , "start_date": iso8601Format startDate
        , "end_date": iso8601Format endDate
        }
    }
  response :: Response <- fromJson json
  pure response

buildTraceData :: Response -> Array (Options TraceData)
buildTraceData (Response { series }) = do
  Series { label, domain, codomain } <- series
  pure $
    x := domain
      <> case codomain of
        IntCodomain ys -> y := ys
        StringCodomain ys -> y := ys
      <> name := label
      <> typ := "scatter"
      <> line := (shape := Hv)
      <> mode := "lines"

draw :: forall m. MonadAff m => MonadState State m => m Unit
draw = do
  { clubName, clubNumber, metrics, startDate, endDate, plot } <- H.get
  clubData <- liftAff $ fetchClubData clubNumber metrics startDate endDate
  let traceData = buildTraceData clubData
  case plot of
    Just p -> void $ liftEffect $ updatePlot p traceData $ title := clubName
    Nothing -> do
      p <- liftEffect $ newPlot (chartDivId clubNumber) traceData $ title := clubName
      H.modify_ \s -> s { plot = Just p }
