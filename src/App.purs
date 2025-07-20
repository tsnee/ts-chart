module App (State, app) where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Argonaut.Encode (toJsonString)
import Data.Array (foldl, snoc)
import Data.Const (Const)
import Data.Date (Date, adjust)
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Options (Options, (:=))
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Halogen as H
import Halogen.Aff.Util (runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Undefined (undefined)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (fromNode, id)
import Web.DOM.Node (Node(..))
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.Common (ClassName(..))
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (HTMLElement, fromElement, toElement)
import Web.HTML.Window (document)

import DateComponent as DC
import ClubComponent as CC
import Plotly.DivId (DivId(..))
import Plotly.Layout (title)
import Plotly.Plotly (newPlot)
import Plotly.Shape (Shape(..))
import Plotly.TraceData (TraceData, line, mode, name, typ, x, y)
import Plotly.Line (shape)
import Types (ClubId(..), ClubMetadataResponse(..), DateChangedEvent(..), StartOrEnd(..))

type State =
  { clubs :: Array ClubId
  , metrics :: Array String
  , startDate :: Date
  , endDate :: Date
  }

data Action
  = Initialize
  | ChangeMetrics (Array String)
  | ChangeStartDate Date
  | ChangeEndDate Date

getElementsBySelector :: QuerySelector -> Effect (Array HTMLElement)
getElementsBySelector q = do
  win <- window
  doc <- document win
  let p = toParentNode $ toDocument doc
  nodeList <- querySelectorAll q p
  nodeArray <- toArray nodeList
  pure $ foldl f [] nodeArray
  where
  f :: Array HTMLElement -> Node -> Array HTMLElement
  f hs n = do
    e <- fromMaybe $ fromNode n
    h <- fromMaybe $ fromElement e
    snoc hs h

createComponents :: Date -> Date -> Array HTMLElement -> Array HTMLElement -> Effect Unit
createComponents startDate endDate [dateRange] clubDivs = runHalogenAff $ do
  emitter <- populateDateDiv startDate endDate dateRange
  traverse_ (populateClubDiv startDate endDate emitter) clubDivs
createComponents _ _ _ _ = log "This HTML file must contain exactly one div with id date-range."

populateDateDiv :: Date -> Date -> HTMLElement -> Aff (HS.Emitter DateChangedEvent)
populateDateDiv startDate endDate parent = do
  { messages: startEmitter } <- runUI DC.component { startOrEnd: Start, date: startDate } parent
  { messages: endEmitter } <- runUI DC.component { startOrEnd: End, date: endDate } parent
  { emitter, listener } <- liftEffect HS.create :: forall a. Aff (HS.SubscribeIO a)
  void $ liftEffect $ HS.subscribe startEmitter $ HS.notify listener
  void $ liftEffect $ HS.subscribe endEmitter $ HS.notify listener
  pure emitter

populateClubDiv :: Date -> Date -> HS.Emitter DateChangedEvent -> HTMLElement -> Aff Unit
populateClubDiv startDate endDate emitter clubDiv = do
  clubNumberString <- liftEffect $ id $ toElement clubDiv
  case fromString clubNumberString of
    Nothing -> log $ "Cannot convert '" <> show clubNumberString <> "' to a club number."
    Just n -> do
      let clubNumber = ClubId n
          metrics = [ "ActiveMembers", "MembershipBase", "NewMembers" ]
      clubName <- fetchClubName clubNumber
      { query } <- runUI CC.component { clubName, clubNumber, metrics, startDate, endDate } clubDiv
      let tell :: DateChangedEvent -> CC.Query _
          tell = H.mkTell <<< CC.DateChangedQuery
          f :: DateChangedEvent -> Effect ?_
          f = query <<< tell
      void $ liftEffect $ HS.subscribe emitter f

fetchClubName ∷ ClubId → Aff String
fetchClubName (ClubId clubNumber) = do
  { json } ← fetch ("https://api.brightringsoftware.com/clubs/" <> show clubNumber) { method: GET }
  response ∷ ClubMetadataResponse ← fromJson json
  let ClubMetadataResponse { club_name } = response
  pure club_name

app :: Effect Unit
app = do
  endDate <- nowDate
  let startDate = fromMaybe endDate $ adjust endDate (Days (-180.0 :: Number))
  dateRange <- getElementsBySelector $ QuerySelector "div#date-range"
  clubDivs <- getElementsBySelector $ QuerySelector "div.club"
  createComponents startDate endDate dateRange clubDivs
