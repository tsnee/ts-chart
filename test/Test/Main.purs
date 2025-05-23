module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)

import Test.ColorScale (tests) as ColorScaleSuite
import Test.Line (tests) as Line

main :: Effect Unit
main = runTest do
  ColorScaleSuite.tests
  Line.tests
