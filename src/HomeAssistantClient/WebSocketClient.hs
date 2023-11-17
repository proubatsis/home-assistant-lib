module HomeAssistantClient.WebSocketClient where
    import HomeAssistantClient.Dsl (HomeAssistantClient, HomeAssistantClientInstruction(..), ServiceInfo (..))
    import HomeAssistantEnv (HomeAssistantEnv(..))
    import HAStates.HomeAssistantState (HomeAssistantState)
    import Control.Monad.Free (Free(..))
    import Control.Monad.Reader (ReaderT (runReaderT), asks, liftIO)
    import Control.Monad.State (StateT, get, put, runStateT)
    import Data.ByteString.Char8 (pack)
    import Data.ByteString.Lazy (ByteString)
    import Data.Aeson (encode, encode, decode)
    import Data.Maybe (fromMaybe)
    import Control.Concurrent (threadDelay)
    import qualified Network.WebSockets as WS
    import Data.Map (Map)
    import qualified Data.Map as Map

    data WebSocketClientState = WebSocketClientState    { currentId :: Int }
    data WebSocketEnv = WebSocketEnv    { webSocketConnection :: WS.Connection
                                        , homeAssistantEnv :: HomeAssistantEnv
                                        }
    type HomeAssistantWebSocketClientContext = ReaderT WebSocketEnv (StateT WebSocketClientState IO)

    authenticate :: HomeAssistantEnv -> WS.Connection -> IO ()
    authenticate env connection = do
        WS.sendTextData connection (encode $ Map.fromList [("type", "auth"), ("access_token", homeAssistantBearerToken env)])
        waitForAuthOkMsg connection

    runHomeAssistantClient :: HomeAssistantEnv -> HomeAssistantClient a -> WS.Connection -> IO a
    runHomeAssistantClient env client connection = do
        let webSocketEnv = WebSocketEnv { webSocketConnection = connection
                                        , homeAssistantEnv = env
                                        }
        let webSocketClientState = WebSocketClientState { currentId = 1 }
        let result = runReaderT (interpretHomeAssistantClient client) webSocketEnv
        (result, _ ) <- runStateT result webSocketClientState
        return result

    waitForAuthOkMsg :: WS.Connection -> IO ()
    waitForAuthOkMsg connection = do
        msg <- WS.receiveData connection
        let msgMap = (decode msg :: Maybe (Map String String))
        let msgType = Map.lookup "type" (fromMaybe Map.empty msgMap)

        case msgType of
            Just "auth_ok" -> return ()
            _ -> do
                threadDelay 1000000
                waitForAuthOkMsg connection

    interpretHomeAssistantClient :: HomeAssistantClient a -> HomeAssistantWebSocketClientContext a
    interpretHomeAssistantClient (Pure a) = return a
    interpretHomeAssistantClient (Free instruction) = do
        next <- interpretHomeAssistantClientInstruction instruction
        interpretHomeAssistantClient next

    interpretHomeAssistantClientInstruction :: HomeAssistantClientInstruction next -> HomeAssistantWebSocketClientContext next
    interpretHomeAssistantClientInstruction (CallService service next) = do
        callId <- nextId
        let msg = encode $ serviceWebsocketJSON service callId
        connection <- asks webSocketConnection
        liftIO $ WS.sendTextData connection msg
        return (next ())
    interpretHomeAssistantClientInstruction _ = error "Not implemented"
    
    nextId :: HomeAssistantWebSocketClientContext Int
    nextId = do
        state <- get
        let id = currentId state
        put $ state { currentId = id + 1 }
        return id
