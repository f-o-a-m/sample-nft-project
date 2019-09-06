module SignalMarket.Indexer.IndexerM where

import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Reader        (MonadReader, ReaderT, runReaderT)
import           SignalMarket.Indexer.Config (IndexerConfig)

newtype IndexerM a = IndexerM
  { unIndexerM :: ReaderT IndexerConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader IndexerConfig, MonadThrow)

runIndexerM :: IndexerConfig -> (forall a. IndexerM a -> IO a)
runIndexerM cfg = flip runReaderT cfg . unIndexerM
