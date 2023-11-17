{-# LANGUAGE OverloadedStrings #-}

module HAStates.HomeAssistantState where

    import Data.Map (Map)
    import Data.Text (unpack)
    import Data.Scientific (toRealFloat)
    import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
    import qualified Data.Aeson as Aeson
    import qualified Data.Vector as Vector

    data StateAttribute = StringAttribute String
                        | IntAttribute Int
                        | FloatAttribute Float
                        | BoolAttribute Bool
                        | ListAttribute [StateAttribute]
                        | UnsupportedAttribute
                        deriving (Show, Eq)

    instance FromJSON StateAttribute where
        parseJSON (Aeson.String s) = return $ StringAttribute (unpack s)
        parseJSON (Aeson.Number n) = return $ case properFraction n of
                                                (i, 0) -> IntAttribute i
                                                _ -> FloatAttribute $ toRealFloat n
        parseJSON (Aeson.Bool b) = return $ BoolAttribute b
        parseJSON (Aeson.Array a) = do
            let list = Vector.toList a
            listAttributes <- mapM parseJSON list
            return $ ListAttribute listAttributes
        parseJSON _ = return UnsupportedAttribute

    data HomeAssistantState = HomeAssistantState    { entityId :: String
                                                    , state :: String
                                                    , attributes :: Map String StateAttribute
                                                    , lastChanged :: String
                                                    } deriving (Show, Eq)

    instance FromJSON HomeAssistantState where
        parseJSON = withObject "HomeAssistantState" $ \o -> do
            entityId' <- o .: "entity_id"
            state' <- o .: "state"
            attributes' <- o .: "attributes"
            lastChanged' <- o .: "last_changed"
            return HomeAssistantState { entityId = entityId'
                                      , state = state'
                                      , attributes = attributes'
                                      , lastChanged = lastChanged'
                                      }
