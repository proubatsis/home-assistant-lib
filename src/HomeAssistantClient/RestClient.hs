{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.RestClient where

    import HomeAssistantClient.Dsl (HomeAssistantClient, HomeAssistantClientInstruction(..), ServiceInfo (..))
    import HomeAssistantEnv (HomeAssistantEnv(..))
    import Control.Monad.Free (Free(..))
    import Control.Monad.Reader (ReaderT, ask, liftIO)
    import Network.HTTP.Client (parseRequest, httpLbs, method, requestBody, requestHeaders, Request, RequestBody(RequestBodyLBS), newManager, defaultManagerSettings)
    import Data.ByteString.Char8 (pack)
    import Data.ByteString.Lazy (ByteString)
    import Data.Aeson (encode, encode)
    
    type HomeAssistantRestClientContext = ReaderT HomeAssistantEnv IO

    interpretHomeAssistantClient :: HomeAssistantClient a -> HomeAssistantRestClientContext a
    interpretHomeAssistantClient (Pure a) = return a
    interpretHomeAssistantClient (Free instruction) = do
        next <- interpretHomeAssistantClientInstruction instruction
        interpretHomeAssistantClient next

    interpretHomeAssistantClientInstruction :: HomeAssistantClientInstruction next -> HomeAssistantRestClientContext next
    interpretHomeAssistantClientInstruction (CallService service next) = do
        env <- ask
        let url = homeAssistantUrl env ++ "/api/services/" ++ serviceDomain service ++ "/" ++ serviceAction service
        let bearerToken = homeAssistantBearerToken env
        let json = encode $ serviceRestJSON service

        request <- liftIO (buildRequest url json bearerToken)
        manager <- liftIO (newManager defaultManagerSettings)
        _ <- liftIO (httpLbs request manager)

        return (next ())
    interpretHomeAssistantClientInstruction (HomeAssistantNoOp next) = return (next ())

    buildRequest :: String -> ByteString -> String -> IO Request
    buildRequest url json bearerToken = do
        request <- parseRequest url
        return $ request { method = "POST"
                         , requestBody = RequestBodyLBS json
                         , requestHeaders = [ ("Content-Type", "application/json")
                                            , ("Authorization", pack $ "Bearer " ++ bearerToken)
                                            ]
                         }
