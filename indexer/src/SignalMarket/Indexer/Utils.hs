module SignalMarket.Indexer.Utils where

import Control.Monad.IO.Class (liftIO)
import           Data.Int                        (Int64)
import           Data.Profunctor.Product.Default (Default)
import           Data.String                     (fromString)
import           Database.PostgreSQL.Simple      (Connection)
import           Katip                           as K
import           Opaleye                         (Table, ToFields, constant,
                                                  runInsertMany)
import           SignalMarket.Indexer.Class
import           SignalMarket.Indexer.Types (Event, mkEvent)
import           SignalMarket.Indexer.IndexerM (IndexerM, runIndexerM)
import           SignalMarket.Indexer.Config (IndexerConfig)
import Network.Ethereum.Api.Provider (Web3)
import Data.Solidity.Prim.Address (Address)
import Network.Ethereum.Api.Types (Filter(..), Change)
import qualified Data.Default as D
import Control.Monad.Reader (ReaderT, ask)
import Network.Ethereum.Web3 (EventAction(..))
import Network.Ethereum.Contract.Event.MultiFilter (Handler(..))
import Data.Proxy

insert
  :: Default ToFields haskells fields
  => MonadPG m
  => K.Katip m
  => K.KatipContext m
  => K.LogItem haskells
  => Table fields fields
  -> haskells
  -> m ()
insert table a = do
  n <- runDB $ \conn ->
    runInsertMany conn table [constant a]
  if n == 1
    then K.katipAddContext a $ logFM DebugS "Inserted"
    else K.logFM K.ErrorS $ fromString ("Inserted " <> show n <> " rows, expected 1.")

makeFilter
  :: forall e. 
     D.Default (Filter e)
  => Proxy e
  -> Address
  -> Filter e
makeFilter _ addr = (D.def :: Filter e) { filterAddress = Just [addr]}

makeHandler
  :: IndexerConfig
  -> (Event e -> IndexerM ())
  -> Handler (ReaderT Change Web3 EventAction) e
makeHandler cfg h = H $ \e -> do
  change <- ask
  liftIO $ runIndexerM cfg $ do
    processRawChange change
    h $ mkEvent change e
    pure ContinueEvent
  where
    processRawChange = undefined