module SignalMarket.Indexer.Utils where


import SignalMarket.Common.Models.RawChange as RawChange
import SignalMarket.Common.EventTypes
  
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Reader                        (ReaderT, ask)
import qualified Data.Default                                as D
import           Data.Int                                    (Int64)
import           Data.Profunctor.Product.Default             (Default)
import           Data.Proxy
import           Data.Solidity.Prim.Address                  (Address)
import           Data.String                                 (fromString)
import           Database.PostgreSQL.Simple                  (Connection)
import           Katip                                       as K
import           Network.Ethereum.Api.Provider               (Web3)
import           Network.Ethereum.Api.Types                  (Change,
                                                              Filter (..))
import           Network.Ethereum.Contract.Event.MultiFilter (Handler (..))
import           Network.Ethereum.Web3                       (EventAction (..))
import           Opaleye                                     (Table, ToFields,
                                                              constant,
                                                              runInsertMany)
import           SignalMarket.Indexer.Class
import           SignalMarket.Indexer.Config                 (IndexerConfig)
import           SignalMarket.Indexer.IndexerM               (IndexerM,
                                                              runIndexerM)
import           SignalMarket.Indexer.Types                  (Event (..), mkEvent)

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
    let event = mkEvent change e
    processRawChange event
    h event
    pure ContinueEvent
  where
    processRawChange Event{eventRawEvent} =
      K.katipAddNamespace "RawChange" $ do
        K.katipAddNamespace "ProcessRawChange" $ do
          insert RawChange.rawChangeTable eventRawEvent
