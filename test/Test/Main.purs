module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.TeamCity (teamcityReporter)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Main (move)

main :: Effect Unit
main = launchAff_ $ runSpec [ teamcityReporter ] do
  describe "Coli" do
    it "moves by its velocity" do
      move { pos: {x: 1.0, y: 1.0}, dir: { x: 1.0, y: 1.0 } }
        # shouldEqual { pos: {x: 2.0, y: 2.0}, dir: { x: 1.0, y: 1.0 } }