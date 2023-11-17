module HomeAssistantClient.Dsl where
    import Control.Monad.Free (Free(..))
    import HAServices.Service (Service)
    import qualified HAServices.Service as Service
    import Data.Aeson (Value)
    import HAStates.HomeAssistantState
    
    type EntityId = String
    
    data ServiceInfo = ServiceInfo  { serviceDomain :: String
                                    , serviceAction :: String
                                    , serviceWebhookJSON :: Value
                                    , serviceRestJSON :: Value
                                    }

    data HomeAssistantClientInstruction next    = CallService ServiceInfo (() -> next)
                                                | GetState EntityId (Maybe HomeAssistantState -> next)

    instance Functor HomeAssistantClientInstruction where
        fmap f (CallService service next) = CallService service (f . next)
        fmap f (GetState entityId' next) = GetState entityId' (f . next)

    type HomeAssistantClient a = Free HomeAssistantClientInstruction a

    callService :: (Service s) => s -> HomeAssistantClient ()
    callService service =
        let serviceInfo = ServiceInfo { serviceDomain = Service.serviceDomain service
                                      , serviceAction = Service.serviceAction service
                                      , serviceWebhookJSON = Service.serviceWebhookJSON service
                                      , serviceRestJSON = Service.serviceRestJSON service
                                      }
        in Free (CallService serviceInfo Pure)

    getState :: EntityId -> HomeAssistantClient (Maybe HomeAssistantState)
    getState entityId' = Free (GetState entityId' Pure)
