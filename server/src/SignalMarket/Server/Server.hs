module SignalMarket.Server.Server (server, mkApplication) where

import           Data.Proxy
import           Servant.API                             ((:<|>) (..))
import           Servant.Server
import           Servant.Swagger
import           SignalMarket.Server.API
import           SignalMarket.Server.API.Types           ()
import           SignalMarket.Server.Application         (runAppHandler)
import           SignalMarket.Server.Config              (AppConfig (..))
import           SignalMarket.Server.Server.Config       (configServer)
import           SignalMarket.Server.Server.FoamToken    (foamTokenServer)
import           SignalMarket.Server.Server.Signal       (signalServer)
import           SignalMarket.Server.Server.SignalMarket (signalMarketServer)
import           SignalMarket.Server.Server.SignalToken  (signalTokenServer)
import           SignalMarket.Server.WebSocket           (wsServer)
--------------------------------------------------------------------------------

server :: AppConfig -> Server API
server cfg = hoistServerWithContext api (Proxy :: Proxy '[]) (runAppHandler cfg) $
       configServer
  :<|> foamTokenServer
  :<|> signalServer
  :<|> signalTokenServer
  :<|> signalMarketServer
  :<|> wsServer cfg

swaggerServer :: Server SwaggerAPI
swaggerServer = pure $ toSwagger api

fullServer :: AppConfig -> Server FullAPI
fullServer cfg = server cfg
            :<|> swaggerServer

mkApplication :: AppConfig -> Application
mkApplication cfg = serveWithContext fullApi EmptyContext $ fullServer cfg
