{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.ResultMessage where

    import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

    data ResultMessage a = ResultMessage  { id' :: Int
                                          , type' :: String
                                          , success :: Bool
                                          , result :: [a]
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
