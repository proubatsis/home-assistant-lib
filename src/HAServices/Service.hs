{-# LANGUAGE OverloadedStrings #-}

module HAServices.Service where

    import Data.Aeson (Value, object, (.=))
    import HAServices.ServiceTarget (ServiceTarget, targetIdName, targetIdValue)
    import Data.Aeson.Types (Pair)
    import Data.Aeson.Key (fromString)

    class Service a where
        serviceDomain :: a -> String
        serviceAction :: a -> String
        serviceTarget :: a -> ServiceTarget
        serviceData :: a -> [Pair]

        serviceName :: a -> String
        serviceName service = serviceDomain service ++ "." ++ serviceAction service

        -- The expected JSON format for a service call is different for websockets and REST calls.
        serviceWebsocketJSON :: a -> Int -> Value
        serviceWebsocketJSON service callId = object    [ "id" .= callId
                                                        , "type" .= ("call_service" :: String)
                                                        , "domain" .= serviceDomain service
                                                        , "service" .= serviceAction service
                                                        , "target" .= object [ fromString (targetIdName (serviceTarget service)) .= targetIdValue (serviceTarget service) ]
                                                        , "service_data" .= object (serviceData service)]

        serviceRestJSON :: a -> Value
        serviceRestJSON service = object $ (fromString (targetIdName (serviceTarget service)) .= targetIdValue (serviceTarget service)) : serviceData service
