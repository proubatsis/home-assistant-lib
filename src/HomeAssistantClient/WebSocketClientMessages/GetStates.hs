{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.GetStates where

    import Data.Aeson (ToJSON, toJSON, object, (.=))

    data GetStates = GetStates Int
    instance ToJSON GetStates where
        toJSON (GetStates callId) = object [ "id" .= callId
                                           , "type" .= ("get_states" :: String)
                                           ]
