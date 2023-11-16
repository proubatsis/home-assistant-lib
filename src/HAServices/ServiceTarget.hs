module HAServices.ServiceTarget where

    data ServiceTarget  = Device String
                        | Area String
                        | Entity String

    targetIdName :: ServiceTarget -> String
    targetIdName (Device _) = "device_id"
    targetIdName (Area _) = "area_id"
    targetIdName (Entity _) = "entity_id"

    targetIdValue :: ServiceTarget -> String
    targetIdValue (Device value) = value
    targetIdValue (Area value) = value
    targetIdValue (Entity value) = value
    