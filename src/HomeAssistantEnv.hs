module HomeAssistantEnv where

    data HomeAssistantEnv = HomeAssistantEnv { homeAssistantHost :: String
                                             , homeAssistantPort :: Int
                                             , useHttps :: Bool
                                             , homeAssistantBearerToken :: String
                                             }

    homeAssistantUrl :: HomeAssistantEnv -> String
    homeAssistantUrl env =
        let protocol = if useHttps env then "https://" else "http://"
        in protocol ++ homeAssistantHost env ++ ":" ++ show (homeAssistantPort env)
