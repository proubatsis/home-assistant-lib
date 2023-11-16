{-# LANGUAGE OverloadedStrings #-}

module HAServices.Light where

    import Data.Word (Word8)
    import HAServices.ServiceTarget (ServiceTarget)
    import Data.Aeson (ToJSON, toJSON, object, (.=))
    import Data.Aeson.Types (Pair)
    

    data LightServiceOption     = Transition Int
                                | RgbColor (Word8, Word8, Word8)
                                | BrightnessValue Word8


    data LightService   = TurnOn ServiceTarget [LightServiceOption]
                        | TurnOff ServiceTarget [LightServiceOption]
                        | Toggle ServiceTarget [LightServiceOption]

    instance ToJSON LightService where
        toJSON (TurnOn target options) = object     [ "service" .= ("light.turn_on" :: String)
                                                    , "target" .= toJSON target
                                                    , "data" .= object (convertOptionsToPairs options)
                                                    ]
        toJSON (TurnOff target options) = object    [ "service" .= ("light.turn_off" :: String)
                                                    , "target" .= toJSON target
                                                    , "data" .= object (convertOptionsToPairs options)
                                                    ]
        toJSON (Toggle target options) = object     [ "service" .= ("light.toggle" :: String)
                                                    , "target" .= toJSON target
                                                    , "data" .= object (convertOptionsToPairs options)
                                                    ]

    convertOptionsToPairs :: [LightServiceOption] -> [Pair]
    convertOptionsToPairs = concatMap convertOptionToPairs

    convertOptionToPairs :: LightServiceOption -> [Pair]
    convertOptionToPairs (Transition value) = ["transition" .= value]
    convertOptionToPairs (RgbColor (r, g, b)) = ["rgb_color" .= [r, g, b]]
    convertOptionToPairs (BrightnessValue value) = ["brightness" .= value]
