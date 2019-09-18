module SignalMarket.Indexer.Utils where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Text (Text)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Reader                        (MonadReader, ReaderT, ask)
import qualified Data.Default                                as D
import           Data.Int                                    (Int64)
import           Data.Profunctor.Product.Default             (Default)
import           Data.Proxy
import           Data.Solidity.Prim.Address                  (Address)
import           Data.String                                 (fromString)
import           Database.PostgreSQL.Simple                  (Connection)
import           Katip                                       as K
import           Network.Ethereum.Api.Provider               (Web3)
import           Network.Ethereum.Api.Types                  (Change(..), unQuantity,
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
import           SignalMarket.Indexer.Types                  (Event, mkEvent)
import SignalMarket.Common.Models.Checkpoint as Checkpoint
import SignalMarket.Common.EventTypes (EventID, HexInteger(..))

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

mkOpenCheckpoint
  :: Text
  -> EventID
  -> Change
  -> Checkpoint.Checkpoint
mkOpenCheckpoint n eid Change{..} = Checkpoint
  { name = n
  , logIndex = HexInteger . unQuantity . fromJust $ changeLogIndex
  , blockNumber = HexInteger . unQuantity . fromJust $ changeBlockNumber
  , status = "open"
  , eventID = eid
  }

--fetchMostRecentCheckpoint
--  :: ( MonadPG m
--     , MonadReader IndexerConfig m
--     )
--  => Text
--  -> m (Maybe Checkpoint)
--fetchMostRecentCheckpoint name = do
--  <- runDB $ \conn -> 