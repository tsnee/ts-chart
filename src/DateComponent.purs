module DateComponent (component) where

import Prelude

import Data.Const (Const)
import Data.Date (Date)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Types (StartOrEnd, iso8601Format)

type State = { startOrEnd :: StartOrEnd, date :: Date }

type Input = State

type Output = State

type Query :: forall k. k -> Type
type Query = Const Void

type Slots :: forall k. Row k
type Slots = ()

data Action = Initialize

inputDivId :: StartOrEnd -> String
inputDivId = show

labelText :: StartOrEnd -> String
labelText startOrEnd = show startOrEnd <> " Date:"

component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent componentSpec
  where
  componentSpec :: H.ComponentSpec State Query Action Slots Input Output m
  componentSpec = { initialState, render, eval }

  initialState :: Input -> State
  initialState = identity
  render { startOrEnd, date } = HH.label
    [ HP.for $ inputDivId startOrEnd ]
    [ HH.text $ labelText startOrEnd
    , HH.input
        [ HP.id $ inputDivId startOrEnd
        , HP.type_ HP.InputDate
        , HP.value $ iso8601Format date
        ]
    ]
  eval = H.mkEval H.defaultEval
