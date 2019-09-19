module SignalMarket.Indexer.IndexerM where

import           Control.Monad.Catch           (MonadThrow, try)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (MonadReader, ReaderT, asks,
                                                runReaderT)
import           Network.Ethereum.Api.Provider (runWeb3With)
import           SignalMarket.Common.Class
import           SignalMarket.Indexer.Config   (IndexerConfig (..))

newtype IndexerM a = IndexerM
  { unIndexerM :: ReaderT IndexerConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader IndexerConfig, MonadThrow, MonadIO)

instance MonadPG IndexerM where
    runDB' action = do
      connection <- asks indexerPGConnection
      liftIO . try $ action connection

instance MonadWeb3 IndexerM where
    runWeb3' action = do
      (manager, provider) <- asks indexerCfgWeb3Manager
      liftIO $ runWeb3With provider manager action

runIndexerM :: IndexerConfig -> (forall a. IndexerM a -> IO a)
runIndexerM cfg = flip runReaderT cfg . unIndexerM
