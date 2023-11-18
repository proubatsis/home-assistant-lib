{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.GetStates where

    import Data.Aeson (object, (.=), Value)

    buildGetStatesMessage :: Int -> Value
    buildGetStatesMessage callId = object   [ "id" .= callId
                                            , "type" .= ("get_states" :: String)
                                            ]
