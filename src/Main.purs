module Main where

import Prelude

import Data.Array (foldl, snoc)
import Data.Date (Date, adjust)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse_)
import Data.Unfoldable (fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import Halogen.Aff (runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (fromNode, id)
import Web.DOM.Node (Node)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (HTMLElement, fromElement, toElement)
import Web.HTML.Window (document)

import DateComponent as DC
import PlotlyComponent as PC
import Types (ClubId(..), Organization(..), StartOrEnd(..))

getElementsBySelector :: QuerySelector -> Effect (Array HTMLElement)
getElementsBySelector q = do
  win <- window
  doc <- document win
  let p = toParentNode $ toDocument doc
  nodeList <- querySelectorAll q p
  nodeArray <- toArray nodeList
  pure $ foldl f [] nodeArray where
    f :: Array HTMLElement -> Node -> Array HTMLElement
    f hs n = do
      e <- fromMaybe $ fromNode n
      h <- fromMaybe $ fromElement e
      snoc hs h

populateDateDiv :: Date -> Date -> HTMLElement -> Effect Unit
populateDateDiv startDate endDate parent = do
  runHalogenAff $ runUI DC.component { startOrEnd: Start, date: startDate } parent
  runHalogenAff $ runUI DC.component { startOrEnd: End, date: endDate } parent

populateClubDiv :: Date -> Date -> HTMLElement -> Effect Unit
populateClubDiv startDate endDate clubDiv = do
  clubNumberString <- (id <<< toElement) clubDiv
  case fromString clubNumberString of
    Nothing -> log $ "Cannot convert '" <> show clubNumberString <> "' to a club number."
    Just n -> runHalogenAff $ runUI PC.component { org, metrics, startDate, endDate } clubDiv where
      org = Club $ ClubId n
      metrics = ["ActiveMembers", "MembershipBase", "NewMembers"]

main :: Effect Unit
main = do
  endDate <- nowDate
  let startDate = case adjust (Days (-180.0 :: Number)) endDate of
        Nothing -> endDate
        Just lastMonth -> lastMonth
  dateRanges <- getElementsBySelector $ QuerySelector "div.date-range"
  traverse_ (populateDateDiv startDate endDate) dateRanges
  clubDivs <- getElementsBySelector $ QuerySelector "div.club"
  traverse_ (populateClubDiv startDate endDate) clubDivs
