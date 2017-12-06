module Test.Day04 where

import Day04
import Prelude

import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "Day04" do
    describe "passphrase" do
      it "should create pairs" do
        pairs ["aa", "bb"] `shouldEqual` [Tuple "aa" "bb", Tuple "bb" "aa"]

      it "should validate a passphrase" do
        isValid "aa bb cc dd ee" `shouldEqual` true

      it "should invalidate a passphrase" do
        isValid "aa bb cc dd aa" `shouldEqual` false

      it "should validate a passphrase" do
        isValid "aa bb cc dd aaa" `shouldEqual` true

    describe "passphrase anagram" do
      it "should see an anagram" do
        isAnagram "oiii" "ioii" `shouldEqual` true

      it "should see not an anagram" do
        isAnagram "abba" "caba" `shouldEqual` false

      it "should validate a passphrase" do
        isValidAnagram "abcde fghij" `shouldEqual` true

      it "should invalidate a passphrase" do
        isValidAnagram "abcde xyz ecdab" `shouldEqual` false

      it "should validate a passphrase" do
        isValidAnagram "a ab abc abd abf abj" `shouldEqual` true

      it "should validate a passphrase" do
        isValidAnagram "iiii oiii ooii oooi oooo" `shouldEqual` true

      it "should invalidate a passphrase" do
        isValidAnagram "oiii ioii" `shouldEqual` false

