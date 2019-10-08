module SignalMarket.Server.Server (server, mkApplication) where

import           Control.Lens                            ((&), (.~), (?~))
import           Data.Proxy
import           Data.Swagger
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
  & info.title   .~ "FOAM Signal Market"
  & info.version .~ "0.1.0"
  & schemes ?~ [Http, Http]
--  & tags .~ fromList ["foam"]
--  & host ?~ Host (indexHost swConfig) (Just . fromIntegral $ indexPort swConfig)

fullServer :: AppConfig -> Server FullAPI
fullServer cfg = server cfg
            :<|> swaggerServer

mkApplication :: AppConfig -> Application
mkApplication cfg = serveWithContext fullApi EmptyContext $ fullServer cfg
