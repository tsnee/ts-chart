module Test.Line (tests) where

import Prelude

import Data.Options (Options, options, (:=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal) as Assert

import Plotly.Line (Line, shape)
import Plotly.Shape (Shape(Linear))
import Test.Evaluation (evaluate)

fixture :: Options Line
fixture = shape := Linear

stringified :: String
stringified = """{ "shape": "linear" }"""

tests :: TestSuite
tests =
  suite "Shape downcasing" do
    test "Happy path" do
      { actual, expected } <- evaluate (options fixture) stringified
      Assert.equal expected actual
