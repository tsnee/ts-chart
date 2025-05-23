module Test.Evaluation (Evaluation, evaluate) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Foreign (Foreign)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

type Evaluation = { actual :: String, expected :: String }

evaluate :: Foreign -> String -> Aff Evaluation
evaluate marshalled stringified = liftEffect $ runFn2 _evaluate marshalled stringified

foreign import _evaluate :: Fn2 Foreign String (Effect Evaluation)
