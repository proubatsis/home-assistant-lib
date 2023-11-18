{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.ResultMessage where

    import Data.Aeson (FromJSON, parseJSON, withObject, (.:), Value(Array))
    import qualified Data.Vector as Vector
    
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

    data ResultMessage a = ResultMessage    { id' :: Int
                                            , type' :: String
                                            , success :: Bool
                                            , result :: ResultField a
                                            }
        deriving (Show, Eq)

    instance FromJSON a => FromJSON (ResultMessage a) where
        parseJSON = withObject "ResultMessage" $ \o -> do
            id'' <- o .: "id"
            type'' <- o .: "type"
            success' <- o .: "success"
            result' <- o .: "result"
            return ResultMessage { id' = id''
                                 , type' = type''
                                 , success = success'
                                 , result = result'
                                 }
