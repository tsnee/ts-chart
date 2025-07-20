module DateComponent (Input, component) where

import Prelude

import Data.Const (Const)
import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, EventType(..))
import Web.UIEvent.InputEvent (data_, fromEvent)

import Types (DateChangedEvent(..), StartOrEnd(..), iso8601Format, iso8601Parse)

type State = { startOrEnd :: StartOrEnd, date :: Date }

type Input = { startOrEnd ∷ StartOrEnd, date ∷ Date }

type Output = DateChangedEvent

type Query :: forall k. k -> Type
type Query = Const Void

labelText :: StartOrEnd -> String
labelText startOrEnd = show startOrEnd <> " Date: "

maybeDateFromEvent :: Event -> Maybe Date
maybeDateFromEvent genericEvt = do
  inputEvt <- fromEvent genericEvt
  inputData <- data_ inputEvt
  iso8601Parse inputData

component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState = identity
  render { startOrEnd, date } = HH.label_
    [ HH.text $ labelText startOrEnd
    , HH.input
        [ HP.type_ HP.InputDate
        , HP.value $ iso8601Format date
        , HE.handler (EventType "change") inputChangeHandler
        ]
    ]
  eval = H.mkEval H.defaultEval
  inputChangeHandler genericEvt =
    case maybeDateFromEvent genericEvt of
      Nothing -> pure unit
      Just d -> do
        { startOrEnd } <- H.modify \s -> s { date = d }
        case startOrEnd of
          Start -> H.raise $ StartDateChanged d
          End -> H.raise $ EndDateChanged d
