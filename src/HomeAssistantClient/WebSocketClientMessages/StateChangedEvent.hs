{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.WebSocketClientMessages.StateChangedEvent where

    import HAStates.HomeAssistantState (HomeAssistantState)
    import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

    data StateChangedEvent = StateChangedEvent  { id' :: Int
                                                , entityId :: String
                                                , timeFired :: String
                                                , oldState :: HomeAssistantState
                                                , newState :: HomeAssistantState
                                                }

    instance FromJSON StateChangedEvent where
        -- Example JSON message:
        -- {
        --     "id": 18,
        --     "type": "event",
        --     "event": {
        --         "data": {
        --             "entity_id": "sensor.time",
        --             "old_state": {...},
        --             "new_state": {...},
        --         },
        --         "event_type": "state_changed",
        --         "time_fired": "2019-01-01T00:00:00.000000+00:00",
        --     }
        -- }
        parseJSON = withObject "StateChangedEvent" $ \o -> do
            id'' <- o .: "id"
            event <- o .: "event"
            timeFired' <- event .: "time_fired"
            eventData <- event .: "data"
            entityId' <- eventData .: "entity_id"
            oldState' <- eventData .: "old_state"
            newState' <- eventData .: "new_state"
            return StateChangedEvent { id' = id''
                                     , entityId = entityId'
                                     , timeFired = timeFired'
                                     , oldState = oldState'
                                     , newState = newState'
                                     }
