module Main where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Data.Array (concat)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(POST))
import Data.Int (fromString)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Options (Options, (:=))
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Effect.Exception (Error, message)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Web.DOM.Document (getElementsByClassName)
import Web.DOM.Element (Element, id)
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.Window (document)

import Plotly.DivId (DivId(..))
import Plotly.Layout (title)
import Plotly.Plotly (newPlot)
import Plotly.Shape (Shape(..))
import Plotly.TraceData (TraceData, line, mode, name, typ, x, y)
import Plotly.Line (shape)
import Utils (Codomain(..), Response(..), Series(..))

fetchaff :: Int -> JSDate -> JSDate -> Aff Response
fetchaff clubNumber start end = do
  { json } <- fetch "https://api.brightringsoftware.com/measurements/club"
    { method: POST
    , body: toJsonString
      { "club_number":clubNumber
      , "start_date":start
      , "end_date":end
      , "metrics":["MembershipBase","NewMembers","ActiveMembers"]
      }
    , headers: { "Content-Type": "application/json" }
    }
  response :: Response <- fromJson json
  pure response

stackedLineChart :: Response -> Effect HTMLElement
stackedLineChart (Response{club_number, series}) = do
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
  newPlot (DivId $ show club_number) td layout

createPlot :: Maybe HTMLInputElement -> Maybe HTMLInputElement -> Either Error Response -> Effect Unit
createPlot _ _ (Left err) = log $ message err
createPlot startInputM endInputM (Right resp) = do
  plot <- stackedLineChart resp
  pure Unit

getDatePicker :: String -> Effect (Maybe Element)
getDatePicker id = do
  win <- window
  doc <- document win
  getElementById id $ toDocument doc

getStartDate :: Maybe Element -> Effect Date
getStartDate startInputM = do
  ???

getClubDivs :: Effect (Array Element)
getClubDivs = do
  win <- window
  doc <- document win
  collection <- getElementsByClassName "club" $ toDocument doc
  toArray collection

logBadNumbers :: String -> Effect (Array Int)
logBadNumbers x =
  case fromString x of
    Just i -> pure [i]
    Nothing -> do
      log $ "Cannot convert '" <> show x <> "' to a club number."
      pure []

main :: Effect Unit
main = do
  startInputM <- getDatePicker "start-date"
  startDate <- getStartDate startInputM
  endInputM <- getDatePicker "end-date"
  endDate <- getEndDate endInputM
  clubDivs <- getClubDivs
  clubNumberStrings <- traverse id clubDivs
  clubNumbers <- traverse logBadNumbers clubNumberStrings
  results <- traverse_ (\club -> runAff_ fetchaff club startDate endDate) $ concat clubNumbers
  traverse (createPlot startInputM endInputM) results
