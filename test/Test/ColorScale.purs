module Test.ColorScale (tests) where

import Prelude

import Data.Tuple (Tuple(Tuple))
import Foreign (unsafeToForeign)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal) as Assert

import Plotly.ColorScale (ColorScale(..), toForeign)
import Test.Evaluation (evaluate)

fixture :: ColorScale
fixture = ColorScale [ Tuple 0.0 "red", Tuple 1.0 "yellow" ]

stringified :: String
stringified = """[[0, "red"], [1, "yellow"]]"""

tests :: TestSuite
tests =
  suite "ColorScale serialization" do
    test "Happy path" do
      result <- evaluate (unsafeToForeign (toForeign fixture)) stringified
      Assert.equal result.expected result.actual
