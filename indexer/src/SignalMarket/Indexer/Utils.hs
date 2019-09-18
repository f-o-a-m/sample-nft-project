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
import           Data.String.Conversions                     (cs)
import           Data.Text                                   (Text)
import           Database.PostgreSQL.Simple                  (Connection)
import           Katip                                       as K
import           Network.Ethereum.Api.Provider               (Web3)
import           Network.Ethereum.Api.Types                  (Change (..),
                                                              DefaultBlock (..),
                                                              Filter (..),
                                                              unQuantity)
import           Network.Ethereum.Contract.Event.MultiFilter (Handler (..))
import           Network.Ethereum.Web3                       (EventAction (..))
import           Opaleye                                     (Column, Order,
                                                              SqlBool, Table,
                                                              ToFields,
                                                              constant, desc,
                                                              keepWhen, limit,
                                                              orderBy,
                                                              queryTable,
                                                              restrict,
                                                              runInsertMany,
                                                              runSelect,
                                                              runUpdateReturning,
                                                              (.==))
import           SignalMarket.Common.Config.Types            (DeployReceipt (..),
                                                              HasEventName (..))
import           SignalMarket.Common.EventTypes              (EventID,
                                                              HexInteger (..),
                                                              _HexInteger)
import qualified SignalMarket.Common.Models.Checkpoint       as Checkpoint
import qualified SignalMarket.Common.Models.RawChange        as RawChange
import           SignalMarket.Indexer.Class
import           SignalMarket.Indexer.Config                 (IndexerConfig)
import           SignalMarket.Indexer.IndexerM               (IndexerM,
                                                              runIndexerM)
import           SignalMarket.Indexer.Types                  (Event (..),
                                                              mkEvent)

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
  change@Change{..} <- ask
  let blockNumber' =  HexInteger . unQuantity . fromJust $ changeBlockNumber
      logIndex' =  HexInteger . unQuantity . fromJust $ changeLogIndex
      shouldSkip = if status == "open"
                     then (blockNumber', logIndex') < (blockNumber, logIndex)
                     else (blockNumber', logIndex') <= (blockNumber, logIndex)
  if shouldSkip
    then pure ContinueEvent
    else h e

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
