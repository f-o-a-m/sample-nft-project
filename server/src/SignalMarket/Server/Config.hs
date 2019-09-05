module SignalMarket.Server.Config where

import           Network.Ethereum.Api.Provider    (Provider (..))
import           Network.HTTP.Client              (Manager)
import           Network.HTTP.Client.TLS          (newTlsManager)
import           SignalMarket.Server.Config.Types (Contracts, mkContracts)
import           SignalMarket.Server.Config.Utils (getEnvVarWithDefault,
                                                   makeConfig)

data AppConfig = AppConfig
  { appCfgContracts   :: Contracts
  , appCfgWeb3Manager :: (Provider, Manager)
  }

mkAppConfig :: IO AppConfig
mkAppConfig = do
  contracts <- makeConfig $ mkContracts
  provider <- makeConfig $
    HttpProvider <$> getEnvVarWithDefault "NODE_URL" "http://localhost:8545"
  web3Mgr <- newTlsManager
  return $ AppConfig
    { appCfgContracts = contracts
    , appCfgWeb3Manager = (provider, web3Mgr)
    }
