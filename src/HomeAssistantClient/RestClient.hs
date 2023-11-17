{-# LANGUAGE OverloadedStrings #-}

module HomeAssistantClient.RestClient where

    import HomeAssistantClient.Dsl (HomeAssistantClient, HomeAssistantClientInstruction(..), ServiceInfo (..))
    import HomeAssistantEnv (HomeAssistantEnv(..))
    import HAStates.HomeAssistantState (HomeAssistantState)
    import Control.Monad.Free (Free(..))
    import Control.Monad.Reader (ReaderT (runReaderT), asks, liftIO)
    import Network.HTTP.Client (parseRequest, httpLbs, method, requestBody, requestHeaders, Request, RequestBody(RequestBodyLBS), newManager, defaultManagerSettings, Response (responseBody))
    import Data.ByteString.Char8 (pack)
    import Data.ByteString.Lazy (ByteString)
    import Data.Aeson (encode, encode, decode)

    type HomeAssistantRestClientContext = ReaderT HomeAssistantEnv IO

    runHomeAssistantClient :: HomeAssistantEnv -> HomeAssistantClient a -> IO a
    runHomeAssistantClient env client = runReaderT (interpretHomeAssistantClient client) env

    interpretHomeAssistantClient :: HomeAssistantClient a -> HomeAssistantRestClientContext a
    interpretHomeAssistantClient (Pure a) = return a
    interpretHomeAssistantClient (Free instruction) = do
        next <- interpretHomeAssistantClientInstruction instruction
        interpretHomeAssistantClient next

    interpretHomeAssistantClientInstruction :: HomeAssistantClientInstruction next -> HomeAssistantRestClientContext next
    interpretHomeAssistantClientInstruction (CallService service next) = do
        let endpoint = "/api/services/" ++ serviceDomain service ++ "/" ++ serviceAction service
        let json = encode $ serviceRestJSON service

        request <- buildPostRequest endpoint json
        manager <- liftIO (newManager defaultManagerSettings)
        _ <- liftIO (httpLbs request manager)

        return (next ())
    interpretHomeAssistantClientInstruction (GetState entityId' next) = do
        let endpoint = "/api/states/" ++ entityId'

        request <- buildGetRequest endpoint
        manager <- liftIO (newManager defaultManagerSettings)
        response <- liftIO (httpLbs request manager)

        let state = decode (responseBody response) :: Maybe HomeAssistantState
        return (next state)

    buildGetRequest :: String -> HomeAssistantRestClientContext Request
    buildGetRequest endpoint = do
        baseUrl <- asks homeAssistantUrl
        request <- parseRequest (baseUrl ++ endpoint)
        bearerToken <- asks homeAssistantBearerToken
        return $ request { method = "GET"
                         , requestHeaders = [ ("Content-Type", "application/json")
                                            , ("Authorization", pack $ "Bearer " ++ bearerToken)
                                            ]
                         }

    buildPostRequest :: String -> ByteString -> HomeAssistantRestClientContext Request
    buildPostRequest endpoint json = do
        baseUrl <- asks homeAssistantUrl
        request <- parseRequest (baseUrl ++ endpoint)
        bearerToken <- asks homeAssistantBearerToken
        return $ request { method = "POST"
                         , requestBody = RequestBodyLBS json
                         , requestHeaders = [ ("Content-Type", "application/json")
                                            , ("Authorization", pack $ "Bearer " ++ bearerToken)
                                            ]
                         }
