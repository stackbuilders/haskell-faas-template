{-# LANGUAGE OverloadedStrings #-}
module ServerSpec (spec) where

import qualified Web.Scotty as S
import Server
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec = with (S.scottyApp app) $ do
  describe "POST /" $ do
    context "when the input is non-empty" $
      it "responds with 200" $ do
        post "/" "foo" `shouldRespondWith` 200

    context "when the input is empty" $
      it "responds with 500" $ do
        post "/" "" `shouldRespondWith` 500
