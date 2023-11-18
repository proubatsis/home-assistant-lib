{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.ResultMessage where

    import Data.Aeson (FromJSON, parseJSON, withObject, (.:), decode)
    import Data.ByteString.Lazy (LazyByteString)

    data ResultMessage a = ResultMessage    { resultId :: Int
                                            , resultType :: String
                                            , resultSuccess :: Bool
                                            , result :: Maybe a
                                            }
        deriving (Show, Eq)

    instance FromJSON a => FromJSON (ResultMessage a) where
        parseJSON = withObject "ResultMessage" $ \o -> do
            id'' <- o .: "id"
            type'' <- o .: "type"
            success' <- o .: "success"
            result' <- o .: "result"
            return ResultMessage { resultId = id''
                                 , resultType = type''
                                 , resultSuccess = success'
                                 , result = result'
                                 }

    decodeResultMessage :: FromJSON a => LazyByteString -> Maybe (ResultMessage a)
    decodeResultMessage = decode
