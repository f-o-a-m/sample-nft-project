module SignalMarket.Server.WebSocket (wsApplication) where

import qualified Database.PostgreSQL.Simple          as PG
import           Network.HTTP.Types                  (status400)
import           Network.Wai                         (Application, responseLBS)
import           Network.Wai.Handler.WebSockets      (websocketsOr)
import qualified Network.WebSockets                  as Socket
import           SignalMarket.Common.Config.Logging  (LogConfig)
import           SignalMarket.Server.WebSocket.App   (defaultWSApplet,
                                                      mkWebSocketApp)
import           SignalMarket.Server.WebSocket.Types (mkWebSocketEnv)



wsApplication ::  LogConfig -> PG.Connection -> IO Application
wsApplication logConfig pg = do
  wsEnv <- mkWebSocketEnv pg logConfig
  let wsApp = mkWebSocketApp wsEnv defaultWSApplet
      backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
  pure $ websocketsOr Socket.defaultConnectionOptions wsApp backupApp
