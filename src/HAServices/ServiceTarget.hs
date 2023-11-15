{-# LANGUAGE OverloadedStrings #-}

module HAServices.ServiceTarget where

    import Data.Aeson (ToJSON, toJSON, object, (.=))

    data ServiceTarget  = Device String
                        | Area String
                        | Entity String

    instance ToJSON ServiceTarget where
        toJSON (Device deviceId) = object [ "device_id" .= deviceId ]
        toJSON (Area areaId) = object [ "area_id" .= areaId ]
        toJSON (Entity entityId) = object [ "entity_id" .= entityId ]
