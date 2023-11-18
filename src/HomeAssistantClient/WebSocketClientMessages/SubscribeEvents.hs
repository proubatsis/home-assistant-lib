{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.SubscribeEvents where

    import Data.Aeson (object, (.=), Value)

    data SubscribeEvents = SubscribeStateChangedEvents

    buildWebSocketClientMessageJSON :: SubscribeEvents -> Int -> Value
    buildWebSocketClientMessageJSON SubscribeStateChangedEvents callId = object [ "id" .= callId
                                                                                , "type" .= ("subscribe_events" :: String)
                                                                                , "event_type" .= ("state_changed" :: String)
                                                                                ]
