module SignalMarket.Server.Config
  ( AppConfig(..)
  , mkAppConfig
    -- * ReExports
  , Contracts(..)
  , DeployReceipt(..)

  ) where

import           Control.Lens                       (lens)
import           Data.String.Conversions            (cs)
import           Database.PostgreSQL.Simple         (Connection, connect)
import qualified Database.Redis                     as Redis
import           Network.Ethereum.Api.Provider      (Provider (..))
import           Network.HTTP.Client                (Manager)
import           Network.HTTP.Client.TLS            (newTlsManager)
import           SignalMarket.Common.Config.Logging (HasLogConfig (..),
                                                     LogConfig)
import           SignalMarket.Common.Config.Node    (getNetworkID)
import           SignalMarket.Common.Config.PG      (mkPGConnectInfo)
import           SignalMarket.Common.Config.Redis   (makeRedisConnectInfo)
import           SignalMarket.Common.Config.Types   (Contracts (..),
                                                     DeployReceipt (..),
                                                     mkContracts)
import           SignalMarket.Common.Config.Utils   (getEnvVarWithDefault,
                                                     makeConfig)

data AppConfig = AppConfig
  { appCfgContracts    :: Contracts
  , appCfgWeb3Manager  :: (Provider, Manager)
  , appCfgLogConfig    :: LogConfig
  , appCfgPGConnection :: Connection
  , appCfgRedis        :: Redis.Connection
  }

mkAppConfig :: LogConfig -> IO AppConfig
mkAppConfig lc = do
  provider <- makeConfig $
    HttpProvider <$> getEnvVarWithDefault "NODE_URL" "http://localhost:8545"
  web3Mgr <- newTlsManager
  networkID <- makeConfig $ cs <$> getNetworkID web3Mgr provider
  contracts <- makeConfig $ mkContracts networkID
  pg <- makeConfig mkPGConnectInfo >>= connect
  redis <- makeRedisConnectInfo  >>= Redis.connect
  return $ AppConfig
    { appCfgContracts = contracts
    , appCfgWeb3Manager = (provider, web3Mgr)
    , appCfgLogConfig = lc
    , appCfgPGConnection = pg
    , appCfgRedis = redis
    }

instance HasLogConfig AppConfig where
  logConfig = lens g s
    where
      g = appCfgLogConfig
      s cfg lc = cfg {appCfgLogConfig = lc}
