module SignalMarket.Indexer.Utils where

import           Control.Arrow                               (returnA)
import           Control.Lens                                ((^.))
import           Control.Monad.Catch                         (MonadThrow)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Reader                        (MonadReader,
                                                              ReaderT, ask)
import qualified Data.Default                                as D
import           Data.Int                                    (Int64)
import           Data.Maybe                                  (fromJust)
import           Data.Profunctor.Product.Default             (Default)
import           Data.Proxy
import           Data.Solidity.Prim.Address                  (Address)
import           Data.String                                 (fromString)
import           Data.Text                                   (Text)
import           Database.PostgreSQL.Simple                  (Connection)
import           Katip                                       as K
import           Network.Ethereum.Api.Provider               (Web3)
import           Network.Ethereum.Api.Types                  (Change (..),
                                                              Filter (..),
                                                              unQuantity)
import           Network.Ethereum.Contract.Event.MultiFilter (Handler (..))
import           Network.Ethereum.Web3                       (EventAction (..))
import           Opaleye                                     (Order, Table, SqlBool, 
                                                              ToFields, Column,
                                                              constant, desc,
                                                              limit, orderBy,
                                                              queryTable, runUpdate, 
                                                              restrict, keepWhen,
                                                              runInsertMany,
                                                              runSelect, (.==))
import           SignalMarket.Common.Config.Types            (HasEventName(..))
import           SignalMarket.Common.EventTypes              (EventID, _HexInteger,
                                                              HexInteger (..))
import qualified SignalMarket.Common.Models.Checkpoint       as Checkpoint
import           SignalMarket.Indexer.Class
import           SignalMarket.Indexer.Config                 (IndexerConfig)
import           SignalMarket.Indexer.IndexerM               (IndexerM,
                                                              runIndexerM)
import           SignalMarket.Indexer.Types                  (Event, mkEvent)
import Data.String.Conversions (cs)

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
mkOpenCheckpoint n eid Change{..} = Checkpoint.Checkpoint
  { name = n
  , logIndex = HexInteger . unQuantity . fromJust $ changeLogIndex
  , blockNumber = HexInteger . unQuantity . fromJust $ changeBlockNumber
  , status = "open"
  , eventID = eid
  }

closeCheckpoint
  :: MonadPG m
  => EventID
  -> m Int64
closeCheckpoint eid = runDB $ \conn -> do
  let updater :: Checkpoint.CheckpointPG -> Checkpoint.CheckpointPG
      updater a = a { Checkpoint.status = constant ("closed" :: Text) }
      p :: Checkpoint.CheckpointPG -> Column SqlBool
      p = \a -> Checkpoint.eventID a .== constant eid
  runUpdate conn Checkpoint.checkpointTable updater p


fetchMostRecentCheckpoint
  :: ( MonadPG m
     , MonadThrow m
     , MonadReader IndexerConfig m
     )
  => Text
  -> m (Maybe Checkpoint.Checkpoint)
fetchMostRecentCheckpoint name =
  let order :: Order Checkpoint.CheckpointPG
      order = desc Checkpoint.blockNumber <>
              desc Checkpoint.logIndex
      q = limit 1 . orderBy order $ proc () -> do
            cp <- queryTable Checkpoint.checkpointTable -< ()
            restrict -<  Checkpoint.name cp .== constant name
            returnA -< cp
  in queryMaybe $ \conn -> runSelect conn q

modifyHandlerWithCheckpoint
  :: Checkpoint.Checkpoint
  -> Handler (ReaderT Change Web3 EventAction) e
  -> Handler (ReaderT Change Web3 EventAction) e
modifyHandlerWithCheckpoint Checkpoint.Checkpoint{..} (H h) = H $ \e -> do
  Change{..} <- ask
  let blockNumber' =  HexInteger . unQuantity . fromJust $ changeBlockNumber
      logIndex' =  HexInteger . unQuantity . fromJust $ changeLogIndex
      shouldSkip = if status == "open" 
                     then (blockNumber', logIndex') < (blockNumber, logIndex)
                     else (blockNumber', logIndex') <= (blockNumber, logIndex)
  if shouldSkip
    then pure ContinueEvent
    else h e

makeFilterHandlerPair
  :: forall e.
     ( D.Default (Filter e)
     , HasEventName e
     )
  => Address
  -> (Event e -> IndexerM ())
  -> IndexerM (Filter e, Handler (ReaderT Change Web3 EventAction) e)
makeFilterHandlerPair addr h = do
  indexerCfg <- ask
  let name = eventName (Proxy :: Proxy e)
  mCheckpoint <- fetchMostRecentCheckpoint name
  case mCheckpoint of
    Nothing -> K.logFM K.InfoS . fromString $ 
      "No checkpoint found for " <> cs name <> ", indexing from contract creation."
    Just cp -> K.katipAddContext cp $ K.logFM K.InfoS . fromString $
      "Indexing " <> cs name <> " from checkpoint."
  let modifyWithCheckpoint :: Handler (ReaderT Change Web3 EventAction) e 
                           -> Handler (ReaderT Change Web3 EventAction) e
      modifyWithCheckpoint = maybe id modifyHandlerWithCheckpoint mCheckpoint
      handler = modifyWithCheckpoint $ makeHandler indexerCfg h
      fltr = makeFilter (Proxy :: Proxy e) addr
  pure (fltr, handler)