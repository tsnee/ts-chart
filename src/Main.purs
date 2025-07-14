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

import PlotlyComponent (component)
import Types (ClubId(..), Organization(..))

getClubDivs :: Effect (Array HTMLElement)
getClubDivs = do
  log "getClubDivs"
  win <- window
  doc <- document win
  let p = toParentNode $ toDocument doc
  nodeList <- querySelectorAll (QuerySelector "div.club") p
  nodeArray <- toArray nodeList
  pure $ foldl f [] nodeArray where
    f :: Array HTMLElement -> Node -> Array HTMLElement
    f hs n = do
      e <- fromMaybe $ fromNode n
      h <- fromMaybe $ fromElement e
      snoc hs h

logBadNumbers :: String -> Effect (Array Int)
logBadNumbers x =
  case fromString x of
    Just i -> pure [i]
    Nothing -> do
      log $ "Cannot convert '" <> x <> "' to a club number."
      pure []

populateClubDiv :: Date -> Date -> HTMLElement -> Effect Unit
populateClubDiv startDate endDate clubDiv = do
  log $ "populateClubDiv " <> show startDate <> ", " <> show endDate
  clubNumberString <- (id <<< toElement) clubDiv
  log $ "populateClubDiv - " <> clubNumberString
  case fromString clubNumberString of
    Nothing -> log $ "Cannot convert '" <> show clubNumberString <> "' to a club number."
    Just n -> runHalogenAff $ runUI component { org, metrics, startDate, endDate } clubDiv where
      org = Club $ ClubId n
      metrics = ["ActiveMembers", "MembershipBase", "NewMembers"]

main :: Effect Unit
main = do
  clubDivs <- getClubDivs
  endDate <- nowDate
  let startDate = case adjust (Days (-180.0 :: Number)) endDate of
        Nothing -> endDate
        Just lastMonth -> lastMonth
  traverse_ (populateClubDiv startDate endDate) clubDivs
