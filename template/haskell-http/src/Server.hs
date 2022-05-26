{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Server (server, app) where

import Control.Exception (ErrorCall (..), Handler (..), SomeException, catches)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import Handler (handle)
import Network.HTTP.Types.Status
import qualified Web.Scotty as S

server :: Int -> IO ()
server port =
  S.scotty port app

app :: S.ScottyM ()
app = S.matchAny (S.regex ".*") $ do
  --myContext <- liftIO getContext
  request <- S.request
  body <- S.body
  liftIO (handleWithError request body) >>= \case
    Right success -> S.json success
    Left err -> do
      S.status status500
      S.text . T.pack $ err
  where
    handleWithError req bs =
      (Right <$> handle req bs)
        `catches` [ Handler (\(ErrorCall s) -> return (Left s)),
                    Handler (\(ex :: SomeException) -> return (Left $ show ex))
                  ]
