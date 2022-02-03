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
import qualified Text.PrettyPrint as T


type ReqBody = ByteString

handle :: Request -> ReqBody -> IO T.Text 
handle _ bs = do
  if LBS.null bs
     then error "Empty payload"
     else return $ either errorResponse successResponse (fromByteString bs)

fromByteString :: ByteString -> Either UnicodeException T.Text
fromByteString = decodeUtf8' . LBS.toStrict

errorResponse :: UnicodeException -> T.Text
errorResponse a = "Error"

successResponse :: T.Text -> T.Text
successResponse a = "OK"
