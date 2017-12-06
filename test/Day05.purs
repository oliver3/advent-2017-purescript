module Test.Day05 where

import Day05
import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "Day05" do
    describe "Escape the maze" do
      it "should escape the maze" do
        escape [0, 3, 0, 1, -3] `shouldEqual` 5

      it "should escape the maze" do
        escape' [0, 3, 0, 1, -3] `shouldEqual` 10


