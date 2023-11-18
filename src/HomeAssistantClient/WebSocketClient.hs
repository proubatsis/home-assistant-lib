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
    import HomeAssistantClient.WebSocketClientMessages.StateChangedEvent (StateChangedEvent)
    import Network.Socket (withSocketsDo)
    import HomeAssistantClient.WebSocketClientMessages.SubscribeEvents (buildWebSocketClientMessageJSON, SubscribeEvents(SubscribeStateChangedEvents))
    import HomeAssistantClient.WebSocketClientMessages.ResultMessage (ResultMessage(ResultMessage))
    import qualified HomeAssistantClient.WebSocketClientMessages.ResultMessage as ResultMessage
    
    data WebSocketClientState = WebSocketClientState    { currentId :: Int }
    data WebSocketEnv = WebSocketEnv    { webSocketConnection :: WS.Connection
                                        , homeAssistantEnv :: HomeAssistantEnv
                                        }
    type HomeAssistantWebSocketClientContext = ReaderT WebSocketEnv (StateT WebSocketClientState IO)

    eventSubscriptionCallId :: Int
    eventSubscriptionCallId = 1
    initialClientCallId :: Int
    initialClientCallId = 2

    withHomeAssistant :: HomeAssistantEnv -> (StateChangedEvent -> HomeAssistantClient ()) -> IO ()
    withHomeAssistant env callback = do
        withSocketsDo $ WS.runClient (homeAssistantHost env) (homeAssistantPort env) "/api/websocket" (wsClientApp env callback)
        return ()

    wsClientApp :: HomeAssistantEnv -> (StateChangedEvent -> HomeAssistantClient ()) -> WS.ClientApp ()
    wsClientApp env callback connection = do
        authenticate env connection
        subscribeToEvents connection
        putStrLn "Waiting for home assistant messages..."
        let webSocketClientState = WebSocketClientState { currentId = initialClientCallId }
        wsClientApp' env webSocketClientState callback connection
    
    wsClientApp' :: HomeAssistantEnv -> WebSocketClientState -> (StateChangedEvent -> HomeAssistantClient ()) -> WS.ClientApp ()
    wsClientApp' env state callback connection = do
        msg <- WS.receiveData connection
        let stateChangedEvent = (decode msg :: Maybe StateChangedEvent)
        case stateChangedEvent of
            Just event -> do
                (_, state') <- runHomeAssistantClient env state (callback event) connection
                wsClientApp' env state' callback connection
            Nothing -> wsClientApp' env state callback connection
        return ()

    authenticate :: HomeAssistantEnv -> WS.Connection -> IO ()
    authenticate env connection = do
        WS.sendTextData connection (encode $ Map.fromList [("type", "auth"), ("access_token", homeAssistantBearerToken env)])
        waitForAuthOkMsg connection

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

    subscribeToEvents :: WS.Connection -> IO ()
    subscribeToEvents connection = do
        WS.sendTextData connection (encode $ buildWebSocketClientMessageJSON SubscribeStateChangedEvents eventSubscriptionCallId)
        waitForSubscribeEventsOkMsg eventSubscriptionCallId connection
        return ()

    waitForSubscribeEventsOkMsg :: Int -> WS.Connection -> IO ()
    waitForSubscribeEventsOkMsg callId connection = do
        msg <- WS.receiveData connection
        let msgResult = (decode msg :: Maybe (ResultMessage Int))
        case msgResult of
            Just (ResultMessage { ResultMessage.id' = id', ResultMessage.success = True }) ->
                if id' == callId
                    then return ()
                    else do
                        threadDelay 1000000
                        waitForSubscribeEventsOkMsg callId connection
            _ -> do
                threadDelay 1000000
                waitForSubscribeEventsOkMsg callId connection
        

    runHomeAssistantClient :: HomeAssistantEnv -> WebSocketClientState -> HomeAssistantClient a -> WS.Connection -> IO (a, WebSocketClientState)
    runHomeAssistantClient env state client connection = do
        let webSocketEnv = WebSocketEnv { webSocketConnection = connection
                                        , homeAssistantEnv = env
                                        }
        let readerResult = runReaderT (interpretHomeAssistantClient client) webSocketEnv
        (result, state') <- runStateT readerResult state
        return (result, state')

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
