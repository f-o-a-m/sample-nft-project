module SignalMarket.Indexer.Config

  ( IndexerConfig(..)
  , mkIndexerConfig
    -- * ReExports
  , Contracts(..)
  , DeployReceipt(..)

  ) where

import           Control.Lens                       (lens)
import           Data.String.Conversions            (cs)
import           Database.PostgreSQL.Simple         (Connection, connect)
import           Network.Ethereum.Api.Provider      (Provider (..))
import           Network.HTTP.Client                (Manager)
import           Network.HTTP.Client.TLS            (newTlsManager)
import           SignalMarket.Common.Config.Logging (HasLogConfig (..),
                                                     LogConfig)
import           SignalMarket.Common.Config.Node    (getNetworkID)
import           SignalMarket.Common.Config.PG      (mkPGConnectInfo)
import           SignalMarket.Common.Config.Types   (Contracts (..),
                                                     DeployReceipt (..),
                                                     mkContracts)
import           SignalMarket.Common.Config.Utils   (getEnvVarWithDefault,
                                                     makeConfig,
                                                     readEnvVarWithDefault)

data IndexerConfig = IndexerConfig
  { indexerCfgContracts    :: Contracts
  , indexerCfgWeb3Manager  :: (Provider, Manager)
  , indexerLogConfig       :: LogConfig
  , indexerPGConnection    :: Connection
  , indexerMultiFilterOpts :: (Integer, Integer) -- (window, lag)
  }

mkIndexerConfig :: LogConfig -> IO IndexerConfig
mkIndexerConfig lc = do
  provider <- makeConfig $
    HttpProvider <$> getEnvVarWithDefault "NODE_URL" "http://localhost:8545"
  web3Mgr <- newTlsManager
  networkID <- makeConfig $ cs <$> getNetworkID web3Mgr provider
  contracts <- makeConfig $ mkContracts networkID
  mfOpts <- makeConfig $ do
    window <- readEnvVarWithDefault "WINDOW_SIZE" 50
    lag <- readEnvVarWithDefault "LAG" 6
    pure (window, lag)
  pg <- makeConfig mkPGConnectInfo >>= connect
  return $ IndexerConfig
    { indexerCfgContracts = contracts
    , indexerCfgWeb3Manager = (provider, web3Mgr)
    , indexerLogConfig = lc
    , indexerPGConnection = pg
    , indexerMultiFilterOpts = mfOpts
    }

instance HasLogConfig IndexerConfig where
  logConfig = lens g s
    where
      g = indexerLogConfig
      s cfg lc = cfg {indexerLogConfig = lc}
