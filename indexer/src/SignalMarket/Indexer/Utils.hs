module SignalMarket.Indexer.Utils
 ( insert
 , update
 , makeFilterHandlerPair
 ) where

import           Control.Arrow                               (returnA)
import           Control.Monad.Catch                         (MonadThrow)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Reader                        (MonadReader,
                                                              ReaderT, ask)
import qualified Data.Default                                as D
import           Data.Maybe                                  (fromJust)
import           Data.Profunctor.Product.Default             (Default)
import           Data.Proxy
import           Data.String                                 (fromString)
import           Data.String.Conversions                     (cs)
import           Data.Text                                   (Text)
import           Katip                                       as K
import           Network.Ethereum.Api.Provider               (Web3)
import           Network.Ethereum.Api.Types                  (Change (..),
                                                              DefaultBlock (..),
                                                              Filter (..),
                                                              unQuantity)
import           Network.Ethereum.Contract.Event.MultiFilter (Handler (..))
import           Network.Ethereum.Web3                       (EventAction (..))
import           Opaleye                                     (Column,
                                                              FromFields, Order,
                                                              SqlBool, Table,
                                                              ToFields,
                                                              constant, desc,
                                                              limit, orderBy,
                                                              queryTable,
                                                              restrict,
                                                              runInsertMany,
                                                              runSelect,
                                                              runUpdateReturning,
                                                              (.==))
import           SignalMarket.Common.Class
import           SignalMarket.Common.Config.Types            (DeployReceipt (..),
                                                              HasEventName (..))
import           SignalMarket.Common.EventTypes              (EventID,
                                                              HexInteger (..))
import qualified SignalMarket.Common.Models.Checkpoint       as Checkpoint
import qualified SignalMarket.Common.Models.RawChange        as RawChange
import           SignalMarket.Indexer.Config                 (IndexerConfig)
import           SignalMarket.Indexer.IndexerM               (IndexerM,
                                                              runIndexerM)
import           SignalMarket.Indexer.Types                  (Event (..),
                                                              mkEvent)

-- | A generic function to perform inserts into postgres.
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

-- | A generic function to perform updates in postgres.
update
  :: Default ToFields haskells fields
  => Default FromFields fields haskells
  => MonadPG m
  => MonadThrow m
  => Table fields fields
  -> (fields -> fields)
  -> (fields -> Column SqlBool)
  -> m haskells
update table updateF keyF = queryExact $ \conn -> do
  runUpdateReturning conn table updateF keyF id

-- | Try to insert a checkpoint. In the event that it already exists,
-- | just log a warning and ignore the error.
tryInsertCheckpoint
  :: MonadPG m
  => K.Katip m
  => K.KatipContext m
  => Checkpoint.Checkpoint
  -> m ()
tryInsertCheckpoint cp = K.katipAddContext cp $ do
  en <- runDB' $ \conn ->
    runInsertMany conn Checkpoint.checkpointTable [constant cp]
  case en of
    Left e -> K.katipAddContext (SqlErrorCTX e) $
      K.logFM K.WarningS "Failed to open checkpoint"
    Right n ->
       if n == 1
         then K.logFM DebugS "Inserted Checkpoint"
         else K.logFM K.ErrorS $ fromString ("Inserted" <> show n <> " rows, expected 1.")

-- | Make a filter for the given event using the given deploy receipt
-- | to find the address to listen for.
makeFilter
  :: forall e.
     D.Default (Filter e)
  => Proxy e
  -> DeployReceipt
  -> Filter e
makeFilter _ DeployReceipt{..} =
   (D.def :: Filter e) { filterAddress = Just [deployReceiptAddress]
                       , filterFromBlock = mkFromBlock deployReceiptBlockNumber
                       }

-- | Take an event handler that runs in the IndexerM context and run it in a
-- | Web3 context after first processing the RawChange data.
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

-- | Make an 'open' checkpoint with the event metadata.
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

-- | Close a checkpoint with the given eventID.
closeCheckpoint
  :: ( MonadPG m
     , MonadThrow m
     )
  => EventID
  -> m Checkpoint.Checkpoint
closeCheckpoint eid = queryExact $ \conn -> do
  let updater :: Checkpoint.CheckpointPG -> Checkpoint.CheckpointPG
      updater a = a { Checkpoint.status = constant ("closed" :: Text) }
      p :: Checkpoint.CheckpointPG -> Column SqlBool
      p a = Checkpoint.eventID a .== constant eid
  runUpdateReturning conn Checkpoint.checkpointTable updater p id

-- | Fetch the most recent checkpoint for an event using it's name
-- | as the key. This is useful to know where to start the filter for this
-- | event. Note, if you are running this event pipeline for the first time
-- | it will not be there.
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

-- | Transform a handler to ignore events that it has already processed
-- | using the checkpoint as a source of truth.
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

-- | Transform a handler to open a checkpoint, then run the handler, then close the checkpoint.
wrapHandlerWithCheckpointing
  :: forall e.
     (HasEventName e)
  => IndexerConfig
  -> Handler (ReaderT Change Web3 EventAction) e
  -> Handler (ReaderT Change Web3 EventAction) e
wrapHandlerWithCheckpointing cfg (H h) = H $ \e -> do
  change <- ask
  let eventID = eventEventID $ mkEvent change e
  _ <- liftIO $ runIndexerM cfg $ do
     let cp = mkOpenCheckpoint (eventName (Proxy :: Proxy e)) eventID change
     tryInsertCheckpoint cp
  eventAction <- h e
  liftIO $ runIndexerM cfg $ do
    cp <- closeCheckpoint eventID
    K.katipAddContext cp $ K.logFM K.InfoS "Closed Checkpoint"
  return eventAction

-- | Takes a handler for an event and deploy receipt for the contract
-- | emitting that event and constructs a filter and handler in a web3
-- | context. It adds checkpointing to the handler via
-- | 'wrapHandlerWithCheckpointing' and make sure the filter uses the
-- | checkpoint for it's starting place. It also adds any offset caused
-- | by the checkpoint via 'modifyHandlerWithCheckpoint'.
makeFilterHandlerPair
  :: forall e.
     ( D.Default (Filter e)
     , HasEventName e
     )
  => DeployReceipt
  -> (Event e -> IndexerM ())
  -> IndexerM (Filter e, Handler (ReaderT Change Web3 EventAction) e)
makeFilterHandlerPair deployReceipt h = do
  indexerCfg <- ask
  let name = eventName (Proxy :: Proxy e)
  mCheckpoint <- fetchMostRecentCheckpoint name
  case mCheckpoint of
    Nothing -> K.logFM K.InfoS . fromString $
      "No checkpoint found for " <> cs name <> ", indexing from contract creation."
    Just cp -> K.katipAddContext cp $ K.logFM K.InfoS . fromString $
      "Indexing " <> cs name <> " from checkpoint."
  let handlerWithCheckpoint :: Handler (ReaderT Change Web3 EventAction) e
                            -> Handler (ReaderT Change Web3 EventAction) e
      handlerWithCheckpoint = maybe id modifyHandlerWithCheckpoint mCheckpoint
      filterWithCheckpoint :: Filter e -> Filter e
      filterWithCheckpoint = maybe id (\cp f -> f {filterFromBlock = mkFromBlock $ Checkpoint.blockNumber cp}) mCheckpoint

      handler = handlerWithCheckpoint . wrapHandlerWithCheckpointing indexerCfg $ makeHandler indexerCfg h
      fltr = filterWithCheckpoint $ makeFilter (Proxy :: Proxy e) deployReceipt
  pure (fltr, handler)

mkFromBlock :: HexInteger -> DefaultBlock
mkFromBlock (HexInteger n) = BlockWithNumber $ fromInteger n
