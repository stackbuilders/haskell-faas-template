{-# LANGUAGE OverloadedStrings #-}

module HandlerSpec where

import Handler ( handle )
import Test.Hspec


import Network.Wai

spec :: Spec
spec =
  describe "handle" $ do
    it "returns a valid Message with status code 200 " $
      handle defaultRequest "TestString" `shouldReturn` "TestString"

    context "when the string is an invalid UTF-8" $
      it "returns a valid Message with status code 500" $
        let errmsg =  "Cannot decode byte '\\xa0': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
         in handle defaultRequest "\xa0\xa1" `shouldReturn` errmsg

    context "when the input is empty" $
      it "throws an exception" $
        handle defaultRequest "" `shouldThrow` errorCall "Empty payload"
