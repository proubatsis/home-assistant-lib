{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.ResultMessage where

    import Data.Aeson (FromJSON, parseJSON, withObject, (.:), Value(Array), decode)
    import qualified Data.Vector as Vector
    import Data.ByteString.Lazy (LazyByteString)

    data ResultField a = ManyResultsField [a] | SingleResultField (Maybe a)
        deriving (Show, Eq)

    instance FromJSON a => FromJSON (ResultField a) where
        parseJSON (Array v) = do
            let list = Vector.toList v
            listValues <- mapM parseJSON list
            return $ ManyResultsField listValues
        parseJSON v = do
            value <- parseJSON v
            return $ SingleResultField value

    data ResultMessage a = ResultMessage    { resultId :: Int
                                            , resultType :: String
                                            , resultSuccess :: Bool
                                            , result :: ResultField a
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
