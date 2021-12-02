{-# LANGUAGE OverloadedStrings #-}

module HandlerSpec where

import Handler ( Message(statusCode, Message), handle )
import Test.Hspec

import Control.Exception (evaluate)

spec :: Spec
spec =
  describe "handle" $ do
    context "when the string is a valid UTF-8" $
      it "returns a valid Message with status code 200 " $
        handle "TestString" `shouldBe` Message 200 "TestString"

    context "when the string is an invalid UTF-8" $
      it "returns a valid Message with status code 500" $
        statusCode (handle "\xa0\xa1") `shouldBe` 500

    context "when the input is empty" $
      it "throws an exception" $
        evaluate (handle "") `shouldThrow` errorCall "Empty payload"
