module HomeAssistantClient.Dsl where
    import Control.Monad.Free (Free(..))
    import HAServices.Service (Service)
    import qualified HAServices.Service as Service
    import Data.Aeson (Value)
    
    data ServiceInfo = ServiceInfo  { serviceDomain :: String
                                    , serviceAction :: String
                                    , serviceWebhookJSON :: Value
                                    , serviceRestJSON :: Value
                                    }

    data HomeAssistantClientInstruction next    = CallService ServiceInfo (() -> next)
                                                | HomeAssistantNoOp (() -> next)

    instance Functor HomeAssistantClientInstruction where
        fmap f (CallService service next) = CallService service (f . next)
        fmap f (HomeAssistantNoOp next) = HomeAssistantNoOp (f . next)

    type HomeAssistantClient a = Free HomeAssistantClientInstruction a

    callService :: (Service s) => s -> HomeAssistantClient ()
    callService service =
        let serviceInfo = ServiceInfo { serviceDomain = Service.serviceDomain service
                                      , serviceAction = Service.serviceAction service
                                      , serviceWebhookJSON = Service.serviceWebhookJSON service
                                      , serviceRestJSON = Service.serviceRestJSON service
                                      }
        in Free (CallService serviceInfo Pure)

    noOp :: HomeAssistantClient ()
    noOp = Free (HomeAssistantNoOp Pure)
