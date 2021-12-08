{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Server (server, app) where

import Handler (handle)
import qualified Web.Scotty as S

import qualified Data.Text.Lazy as T

import Control.Exception (catches, SomeException, ErrorCall(..), Handler(..))
import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Status

import System.Environment (lookupEnv)
import Data.Maybe

server :: Int -> IO ()
server port =
  S.scotty port app

app :: S.ScottyM ()
app =
  S.matchAny (S.regex ".*") $ do
    myContext <- liftIO getContext
    request <- S.request
    body <- S.body
    liftIO (handleWithError myContext request body) >>= \case
      Right success -> S.json success
      Left err -> do
        S.status status500
        S.text . T.pack $ err
  where
    handleWithError context req bs = (Right <$> handle context req bs)
      `catches` [ Handler (\(ErrorCall s) -> return (Left s))
                , Handler (\(ex :: SomeException) -> return (Left $ show ex))
                ]

getEnvironment :: String -> String -> IO (String, String)
getEnvironment varname defvalue = (varname,) . fromMaybe defvalue <$> lookupEnv varname

envList :: [(String, String)]
envList = [("HOSTNAME", "localhost"), ("TEST","DEFAULT")]

getContext :: IO [(String, String)]
getContext = mapM (uncurry getEnvironment) envList
