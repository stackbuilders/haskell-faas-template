{-# LANGUAGE DeriveGeneric #-}
module Handler (handle, Message (..)) where

import Data.Aeson ( ToJSON )
import GHC.Generics ( Generic )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8')
import Data.Text.Encoding.Error ( UnicodeException (..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Wai

data Message = Message 
  { statusCode :: Int
  , content :: T.Text 
  } deriving (Show, Eq, Generic)

instance ToJSON Message

type EnvContext = [(String, String)]
type ReqBody = ByteString

handle :: EnvContext -> Request -> ReqBody -> IO Message
handle _ _ bs = do
  if LBS.null bs
     then error "Empty payload"
     else return $ either errorResponse successResponse (fromByteString bs)

fromByteString :: ByteString -> Either UnicodeException T.Text
fromByteString = decodeUtf8' . LBS.toStrict

toDecodeError :: UnicodeException -> T.Text
toDecodeError = T.pack . show

errorResponse :: UnicodeException -> Message
errorResponse a = Message { statusCode = 500, content = toDecodeError a }

successResponse :: T.Text -> Message
successResponse a = Message { statusCode = 200, content = a }