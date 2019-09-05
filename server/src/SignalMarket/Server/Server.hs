module SignalMarket.Server.Server where

import Data.Proxy
import Servant.Server
import SignalMarket.Server.Config.Types (Contracts)
import SignalMarket.Server.Config (AppConfig(..), mkAppConfig)
import SignalMarket.Server.Application (AppHandler, runAppHandler)
import SignalMarket.Server.API
import Control.Monad.Reader (asks)

--------------------------------------------------------------------------------

server :: AppConfig -> Server API
server cfg = hoistServerWithContext api (Proxy :: Proxy '[]) (runAppHandler cfg) $
    getContracts

mkApplication :: IO Application
mkApplication = do
  cfg <- mkAppConfig
  pure $ serveWithContext api EmptyContext $ server cfg

--------------------------------------------------------------------------------

getContracts :: AppHandler Contracts
getContracts = asks appCfgContracts