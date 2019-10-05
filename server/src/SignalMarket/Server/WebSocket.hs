module SignalMarket.Server.WebSocket (wsServer) where

import           Network.HTTP.Types                (status400)
import           Network.Wai                       (Application, responseLBS)
import           Network.Wai.Handler.WebSockets    (websocketsOr)
import qualified Network.WebSockets                as Socket
import           Servant.Server
import           SignalMarket.Server.API           (WebSocketAPI)
import           SignalMarket.Server.Application   (AppHandler)
import           SignalMarket.Server.Config        (AppConfig (..))
import           SignalMarket.Server.WebSocket.App (defaultWSApplet,
                                                    mkWebSocketApp)


wsServer :: AppConfig -> ServerT WebSocketAPI AppHandler
wsServer cfg =
  Tagged $ wsApplication cfg

wsApplication :: AppConfig -> Application
wsApplication AppConfig{appCfgLogConfig, appCfgPGConnection} =
  let wsApp = mkWebSocketApp appCfgPGConnection appCfgLogConfig defaultWSApplet
      backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
  in websocketsOr Socket.defaultConnectionOptions wsApp backupApp
