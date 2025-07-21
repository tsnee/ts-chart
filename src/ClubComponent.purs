module ClubComponent (render) where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element (Element, id, setClassName, setId, toNode)
import Web.DOM.Node (appendChild)
import Web.DOM.Text as Text
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

import Plotly.DivId (DivId(..))
import Plotly.Layout (title)
import Plotly.Plotly (newPlot, updatePlot)
import Plotly.Shape (Shape(..))
import Plotly.TraceData (TraceData, line, mode, name, typ, x, y)
import Plotly.Line (shape)
import Types (ChangeEvent(..), ChangeEventListener, ClubDataResponse(..), ClubId(..), ClubMetadataResponse(..), Codomain(..), Series(..), iso8601Format)

type ClubData =
  { name ∷ String
  , number ∷ ClubId
  , div_ ∷ Element
  , data_ ∷ Array Series
  }

render :: Date -> Date -> Array String -> Element -> Aff (Either String ChangeEventListener)
render startDate endDate metrics clubDiv = do
  either <- lookupByDiv startDate endDate metrics clubDiv
  case either of
    Left err -> pure $ Left err
    Right clubData@{number} -> do
      plot <- liftEffect $ draw clubData
      pure $ Right $ change number plot

draw :: ClubData -> Effect Element
draw { name, number, div_, data_ } = do
  win <- window
  htmlDoc <- document win
  let doc = toDocument htmlDoc
      showClubId (ClubId i) = show i
      divId = "chart-for-" <> showClubId number
  h3 <- createElement "h3" doc
  text <- createTextNode name doc
  appendChild (Text.toNode text) $ toNode h3
  appendChild (toNode h3) $ toNode div_
  plot <- createElement "div" doc
  setId divId plot
  setClassName "chart" plot
  appendChild (toNode plot) $ toNode div_
  let traceData = buildTraceData data_
      layout = title := name
  void $ newPlot (DivId divId) traceData layout
  pure plot

change :: ClubId -> Element -> ChangeEvent -> Aff (Either String Unit)
change clubId plotDiv (ChangeEvent startDate endDate metrics) = do
  either <- lookupByClubId startDate endDate clubId metrics plotDiv
  case either of
    Left err -> pure $ Left err
    Right clubData -> liftEffect $ Right <$> redraw clubData plotDiv

redraw :: ClubData -> Element -> Effect Unit
redraw { name, data_ } plot = do
  let traceData = buildTraceData data_
      layout = title := name
  updatePlot plot traceData layout

buildTraceData :: Array Series -> Array (Options TraceData)
buildTraceData series = do
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

lookupByDiv :: Date -> Date -> Array String -> Element -> Aff (Either String ClubData)
lookupByDiv startDate endDate metrics clubDiv = do
  clubNumberString <- liftEffect $ id clubDiv
  case fromString clubNumberString of
    Nothing -> pure $ Left $ "Cannot convert " <> show clubNumberString <> " to a club number."
    Just n -> lookupByClubId startDate endDate (ClubId n) metrics clubDiv

lookupByClubId :: Date -> Date -> ClubId -> Array String -> Element -> Aff (Either String ClubData)
lookupByClubId startDate endDate clubNumber metrics clubDiv = do
  clubName <- fetchClubName clubNumber endDate
  clubData <- fetchClubData clubNumber metrics startDate endDate
  pure $ Right $ { name: clubName, number: clubNumber, div_: clubDiv, data_: clubData }

fetchClubName ∷ ClubId → Date -> Aff String
fetchClubName (ClubId clubNumber) date = do
  let url = "https://api.brightringsoftware.com/clubs/" <> show clubNumber <> "?date=" <> iso8601Format date
  { json } ← fetch url { method: GET }
  response ∷ ClubMetadataResponse ← fromJson json
  let ClubMetadataResponse { club_name } = response
  pure club_name

fetchClubData :: ClubId -> Array String -> Date -> Date -> Aff (Array Series)
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
  response :: ClubDataResponse <- fromJson json
  let ClubDataResponse { series } = response
  pure series
