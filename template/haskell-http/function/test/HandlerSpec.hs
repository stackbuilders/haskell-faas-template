{-# LANGUAGE OverloadedStrings #-}

module HandlerSpec where

import Handler ( Message(statusCode, Message), handle )
import Test.Hspec

import Network.Wai

spec :: Spec
spec =
  context "when the string is a valid UTF-8" $
      it "returns a valid Message with status code 200 " $
        handle defaultRequest "TestString" `shouldReturn` Message 200 "TestString"

    context "when the string is an invalid UTF-8" $
      it "returns a valid Message with status code 500" $
        fmap statusCode (handle defaultRequest "\xa0\xa1") `shouldReturn` 500

    context "when the input is empty" $
      it "throws an exception" $
        handle defaultRequest "" `shouldThrow` errorCall "Empty payload"

