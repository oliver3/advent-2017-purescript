module Test.Day03 where

import Day03
import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "Day03" do
    describe "Grid" do
      it "should create a spiral grid" do
        read (createTurtleGrid increaseValue 25) `shouldEqual` Just 25

      it "should create a spiral grid" do
        (createTurtleGrid increaseValue 25).turtle.position `shouldEqual` Tuple 2 2

      it "should calculate manhattan" do
        manhattan (createTurtleGrid increaseValue 25) `shouldEqual` 4


