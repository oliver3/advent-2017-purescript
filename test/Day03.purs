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
        read (createTurtleGrid increasePreviousValue 25) `shouldEqual` Just 25

      it "should create a spiral grid" do
        (createTurtleGrid increasePreviousValue 25).turtle.position `shouldEqual` Tuple 2 2

      it "should calculate manhattan" do
        manhattan (createTurtleGrid increasePreviousValue 25) `shouldEqual` 4

    describe "Sum of neighbours" do
      it "should create a spiral until at least 800" do
        read (createTurtleGrid sumOfNeighbours 800) `shouldEqual` Just 806


