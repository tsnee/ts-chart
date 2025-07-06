module Test.Evaluation (Evaluation, evaluate) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

type Evaluation = { actual :: String, expected :: String }

evaluate :: forall a. a -> String -> Aff Evaluation
evaluate marshalled stringified = liftEffect $ runFn2 _evaluate marshalled stringified

foreign import _evaluate :: forall a. Fn2 a String (Effect Evaluation)
