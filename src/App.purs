module App (app) where

import Prelude

import Control.Parallel (parSequence)
import Data.Array (foldl, snoc)
import Data.Date (Date, adjust)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable as U
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Effect.Exception (Error, message)
import Effect.Now (nowDate)
import Web.DOM.Document (toNonElementParentNode, toParentNode)
import Web.DOM.Element (Element, fromNode, toEventTarget)
import Web.DOM.Node (Node)
import Web.DOM.NodeList (toArray)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLInputElement as HIE
import Web.HTML.Window (document)

import ClubComponent (render)
import Types (ChangeEvent(..), ChangeEventListener, iso8601Format, iso8601Parse)

startDateId :: String
startDateId = "start-date"

endDateId :: String
endDateId = "end-date"

app :: Effect Unit
app = do
  endDate <- nowDate
  let startDate = fromMaybe endDate $ adjust (Days (-180.0 :: Number)) endDate
      metrics = [ "ActiveMembers", "MembershipBase", "NewMembers" ]
  startDateInputM <- getInputElementById startDateId
  endDateInputM <- getInputElementById endDateId
  case Tuple startDateInputM endDateInputM of
    Tuple (Just startDateInput) (Just endDateInput) -> mkClubComponents startDate startDateInput endDate endDateInput metrics
    _ -> log $ "Could not find date input elements with IDs " <> show startDateId <> " and " <> show endDateId <> " in this HTML."

mkClubComponents :: Date -> HIE.HTMLInputElement -> Date -> HIE.HTMLInputElement -> Array String -> Effect Unit
mkClubComponents startDate startDateInput endDate endDateInput metrics = do
  HIE.setDefaultValue (iso8601Format startDate) startDateInput
  HIE.setDefaultValue (iso8601Format endDate) endDateInput
  clubDivs <- getElementsBySelector $ QuerySelector "div.club"
  let clubAffs :: Array (Aff (Either String ChangeEventListener))
      clubAffs = clubDivs <#> render startDate endDate metrics
      clubsAff :: Aff (Array (Either String ChangeEventListener))
      clubsAff = parSequence clubAffs
      addListener :: Either String ChangeEventListener -> Effect Unit
      addListener (Left err) = log err
      addListener (Right cel) = listenToChangeEvents startDateInput endDateInput metrics cel
      callback :: Either Error (Array (Either String ChangeEventListener)) -> Effect Unit
      callback (Left err) = log $ message err
      callback (Right cels) = traverse_ addListener cels
  runAff_ callback clubsAff

getInputElementById :: String -> Effect (Maybe HIE.HTMLInputElement)
getInputElementById id = do
  win <- window
  doc <- document win
  let p = toNonElementParentNode $ toDocument doc
  el <- getElementById id p
  pure $ HIE.fromElement =<< el

getElementsBySelector :: QuerySelector -> Effect (Array Element)
getElementsBySelector q = do
  win <- window
  doc <- document win
  let p = toParentNode $ toDocument doc
  nodeList <- querySelectorAll q p
  nodeArray <- toArray nodeList
  pure $ foldl f [] nodeArray
  where
  f :: Array Element -> Node -> Array Element
  f es n = do
    e <- U.fromMaybe $ fromNode n
    snoc es e

listenToChangeEvents :: HIE.HTMLInputElement -> HIE.HTMLInputElement -> Array String -> ChangeEventListener -> Effect Unit
listenToChangeEvents startDateInput endDateInput metrics cel = do
  let changeEvent = EventType "change"
      callback :: Either Error (Either String Unit) -> Effect Unit
      callback (Left err) = log $ message err
      callback (Right (Left err)) = log err
      callback (Right (Right _)) = pure unit
      changeChart = const $ do
        newStartValue <- HIE.value startDateInput
        newEndValue <- HIE.value endDateInput
        case Tuple (iso8601Parse newStartValue) (iso8601Parse newEndValue) of
          Tuple (Just newStart) (Just newEnd) -> runAff_ callback $ cel $ ChangeEvent newStart newEnd metrics
          _ -> log $ "Could not parse start date " <> show newStartValue <> " and/or end date " <> show newEndValue
  listener <- eventListener changeChart
  addEventListener changeEvent listener false $ toEventTarget $ HIE.toElement startDateInput
  addEventListener changeEvent listener false $ toEventTarget $ HIE.toElement endDateInput
