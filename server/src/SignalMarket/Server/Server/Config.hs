module SignalMarket.Server.Server.Config (configServer) where

import           Control.Monad.Reader            (asks)
import qualified Katip                           as K
import           Servant.Server
import           SignalMarket.Server.API
import           SignalMarket.Server.Application (AppHandler)
import           SignalMarket.Server.Config      (AppConfig (..), Contracts)

--------------------------------------------------------------------------------

configServer :: ServerT ConfigAPI AppHandler
configServer = getContractsH

getContractsH :: AppHandler Contracts
getContractsH = do
  K.logFM K.DebugS "Fetching contracts"
  asks appCfgContracts
