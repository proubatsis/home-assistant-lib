{-# LANGUAGE OverloadedStrings #-}

module HAServices.Light where

    import Data.Word (Word8)
    import HAServices.ServiceTarget (ServiceTarget)
    import HAServices.Service
    import Data.Aeson ((.=))
    import Data.Aeson.Types (Pair)
    

    data LightServiceOption     = Transition Int
                                | RgbColor (Word8, Word8, Word8)
                                | BrightnessValue Word8


    data LightService   = TurnOn ServiceTarget [LightServiceOption]
                        | TurnOff ServiceTarget [LightServiceOption]
                        | Toggle ServiceTarget [LightServiceOption]

    instance Service LightService where
        serviceDomain _ = "light"
        serviceAction (TurnOn _ _) = "turn_on"
        serviceAction (TurnOff _ _) = "turn_off"
        serviceAction (Toggle _ _) = "toggle"
        serviceTarget (TurnOn target _) = target
        serviceTarget (TurnOff target _) = target
        serviceTarget (Toggle target _) = target
        serviceData (TurnOn _ options) = convertOptionsToPairs options
        serviceData (TurnOff _ options) = convertOptionsToPairs options
        serviceData (Toggle _ options) = convertOptionsToPairs options

    convertOptionsToPairs :: [LightServiceOption] -> [Pair]
    convertOptionsToPairs = concatMap convertOptionToPairs

    convertOptionToPairs :: LightServiceOption -> [Pair]
    convertOptionToPairs (Transition value) = ["transition" .= value]
    convertOptionToPairs (RgbColor (r, g, b)) = ["rgb_color" .= [r, g, b]]
    convertOptionToPairs (BrightnessValue value) = ["brightness" .= value]
