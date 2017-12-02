module Test.Day02 where

import Prelude

import Day02 (checksum, checksumDiv)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "Day02" do
    describe "Checksum" do
      it "should checksum with max min" do
        checksum [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]] `shouldEqual` 18

    describe "Checksum div" do
      it "should checksum with div" do
        checksumDiv [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]] `shouldEqual` 9

