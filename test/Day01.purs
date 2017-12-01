module Test.Day01 where

import Prelude

import Day01 (captcha, captchaHalfway)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "Day01" do
    describe "First star" do
      it "should captcha 1122" do
        captcha "1122" `shouldEqual` 3

      it "should captcha 1111" do
        captcha "1111" `shouldEqual` 4

      it "should captcha 1234" do
        captcha "1234" `shouldEqual` 0

      it "should captcha 91212129" do
        captcha "91212129" `shouldEqual` 9

    describe "Second Star" do
      it "should captchaHalfway 1212" do
        captchaHalfway "1212" `shouldEqual` 6

      it "should captchaHalfway 1221" do
        captchaHalfway "1221" `shouldEqual` 0

      it "should captchaHalfway 123425" do
        captchaHalfway "123425" `shouldEqual` 4

      it "should captchaHalfway 123123" do
        captchaHalfway "123123" `shouldEqual` 12

      it "should captchaHalfway 12131415" do
        captchaHalfway "12131415" `shouldEqual` 4
