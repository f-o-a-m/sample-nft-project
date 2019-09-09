module SignalMarket.Server.Server (server, mkApplication) where

import           Control.Monad.Reader             (asks)
import           Data.Proxy
import qualified Katip                            as K
import           Servant.Server
import           SignalMarket.Common.Config.Utils (makeConfig)
import           SignalMarket.Server.API
import           SignalMarket.Server.Application  (AppHandler, runAppHandler)
import           SignalMarket.Server.Config       (AppConfig (..), Contracts,
                                                   mkAppConfig)
--------------------------------------------------------------------------------

server :: AppConfig -> Server API
server cfg = hoistServerWithContext api (Proxy :: Proxy '[]) (runAppHandler cfg) $
    getContracts

mkApplication :: AppConfig -> Application
mkApplication cfg = serveWithContext api EmptyContext $ server cfg

--------------------------------------------------------------------------------

getContracts :: AppHandler Contracts
getContracts = do
  K.logFM K.DebugS "Fetching contracts"
  asks appCfgContracts
