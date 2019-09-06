module SignalMarket.Server.Server (server, mkApplication) where

import           Control.Monad.Reader            (asks)
import           Data.Proxy
import           Servant.Server
import           SignalMarket.Server.API
import           SignalMarket.Server.Application (AppHandler, runAppHandler)
import           SignalMarket.Server.Config      (AppConfig (..), Contracts,
                                                  mkAppConfig)

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
