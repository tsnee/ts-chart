module PlotlyComponent (Output, Query, component) where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Argonaut.Encode (toJsonString)
import Data.Const (Const)
import Data.Date (Date)
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(..))
import Data.Options (Options, (:=))
import Effect (Effect)
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
import Plotly.Plotly (newPlot)
import Plotly.Shape (Shape(..))
import Plotly.TraceData (TraceData, line, mode, name, typ, x, y)
import Plotly.Line (shape)
import Types (AreaId(..), ClubId(..), Codomain(..), DistrictId(..), DivisionId(..), Organization(..), Response(..), Series(..), iso8601Format)

type State =
  { org :: Organization
  , metrics :: Array String
  , startDate :: Date
  , endDate :: Date
  }

type Input = State

type Output = Void

type Query :: forall k. k -> Type
type Query = Const Void

type Slots :: forall k. Row k
type Slots = ()

data Action
  = Initialize
  | ChangeOrg Organization
  | ChangeMetrics (Array String)
  | ChangeStartDate Date
  | ChangeEndDate Date

headerText :: State -> String
headerText { org: Area (AreaId areaId) } = show areaId
headerText { org: Area AreaNotAssigned } = "Area Not Assigned"
headerText { org: Club (ClubId clubId) } = show clubId
headerText { org: District (DistrictId districtNumber) } = show districtNumber
headerText { org: Division (DivisionId divisionLetter) } = show divisionLetter
headerText { org: Division DivisionNotAssigned } = "Division Not Assigned"

chartDivId :: State -> String
chartDivId { org: Area (AreaId areaId) } = "area-" <> show areaId
chartDivId { org: Area AreaNotAssigned } = "area-not-assigned"
chartDivId { org: Club (ClubId clubId) } = "club-" <> show clubId
chartDivId { org: District (DistrictId districtNumber) } = "district-" <> show districtNumber
chartDivId { org: Division (DivisionId divisionLetter) } = "division-" <> show divisionLetter
chartDivId { org: Division DivisionNotAssigned } = "division-not-assigned"

component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent componentSpec
  where
    componentSpec :: H.ComponentSpec State Query Action Slots Input Output m
    componentSpec = { initialState, render, eval }
    initialState :: Input -> State
    initialState = identity
    render state = HH.div_
      [ HH.h3_ [HH.text $ headerText state ]
      , HH.div
          [ HP.id $ chartDivId state
          , HP.class_ (ClassName "chart")
          ] []
      ]
    eval =
      H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction = case _ of
                       Initialize -> draw
                       ChangeOrg org -> do
                          log $ "changing club to " <> show org
                          H.modify_ \s -> s { org = org }
                          redraw
                       ChangeMetrics metrics -> do
                          log $ "changing metrics to " <> show metrics
                          H.modify_ \s -> s { metrics = metrics }
                          redraw
                       ChangeStartDate startDate -> do
                          log $ "changing start date to " <> show startDate
                          H.modify_ \s -> s { startDate = startDate }
                          redraw
                       ChangeEndDate endDate -> do
                          log $ "changing end date to " <> show endDate
                          H.modify_ \s -> s { endDate = endDate }
                          redraw

draw :: forall m. MonadState State m => MonadAff m => m Unit
draw = do
  { org, metrics, startDate, endDate } <- H.get
  case org of
    Club clubId -> do
      clubData <- liftAff $ fetchaff clubId metrics startDate endDate
      void $ liftEffect $ createPlot clubData
    unexpected -> log $ "This code only supports Clubs, not " <> show unexpected

fetchaff :: forall m. MonadAff m => ClubId -> Array String -> Date -> Date -> m Response
fetchaff (ClubId clubNumber) metrics startDate endDate = liftAff $ do
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

createPlot :: Response -> Effect HTMLElement
createPlot (Response{club_number, series}) = do
  let layout = title := show club_number
      td :: Array (Options TraceData)
      td = do
        Series{ label, domain, codomain } <- series
        pure $
          x := domain
          <> case codomain of
            IntCodomain ys -> y := ys
            StringCodomain ys -> y := ys
          <> name := label
          <> typ := "scatter"
          <> line := (shape := Hv)
          <> mode := "lines"
  newPlot (DivId $ "club-" <> show club_number) td layout

redraw :: forall m. MonadAff m => MonadState State m => m Unit
redraw = pure unit
