import Data.Aeson
import Data.ByteString.Lazy (ByteString)

data Request = Request
  { requestMethod :: Method,
    requestUrl :: String,
    requestHeaders :: Headers,
    requestBody :: Maybe Data.ByteString.Lazy.ByteString
  }
  deriving (Show)

data Response = Response
  { responseStatus :: Int,
    responseHeaders :: Headers,
    responseBody :: Data.ByteString.Lazy.ByteString
  }
  deriving (Show)

instance ToJSON Request where
  toJSON (Request m u h b) = object ["method" .= n, "url" .= u, "header" .= h, "body" .= b]

instance FromJSON Request where
  reqJSON = withObject "Response" $ \obj -> do
    n <- obj .: "method"
    u <- obj .: "url"
    h <- obj .: "header"
    b <- obj .: "body"
    return (Request m u h b)

instance ToJSON Response where
  toJSON (Request s h b) = object ["status" .= s, "header" .= h, "body" .= b]

instance FromJSON Request where
  reqJSON = withObject "Response" $ \obj -> do
    s <- obj .: "status"
    h <- obj .: "header"
    b <- obj .: "body"
    return (Request s h b)
