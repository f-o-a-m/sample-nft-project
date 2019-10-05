module SignalMarket.Server.WebSocket.App where

import           Control.Arrow                           (returnA)
import           Control.Concurrent.Async                (async)
import           Control.Monad                           (forever)
import           Control.Monad.Catch                     (MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Reader                    (MonadReader (..))
import           Control.Monad.Trans                     (lift)
import qualified Data.Aeson                              as A
import           Data.Conduit
import           Data.Conduit.List                       (sourceList)
import           Data.IORef
import qualified Data.List                               as L
import           Data.String                             (fromString)
import           Data.String.Conversions                 (cs)
import           Data.Text                               (Text)
import qualified Database.PostgreSQL.Simple              as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import qualified Katip                                   as K
import qualified Network.WebSockets                      as Socket
import qualified Opaleye                                 as O
import           SignalMarket.Common.Class               (MonadPG (..),
                                                          queryExact)
import           SignalMarket.Common.Config.Logging
import           SignalMarket.Common.EventTypes          (EventID (..),
                                                          parseHexString)
import qualified SignalMarket.Common.Models.RawChange    as RC
import           SignalMarket.Server.WebSocket.Types

mkWebSocketApp
  :: PG.Connection
  -> LogConfig
  -> WSApplet
  -> Socket.ServerApp
mkWebSocketApp pg logCfg WSApplet{clientMsgHandler, msgConduit} pendingConn = do
  socket <- Socket.acceptRequest pendingConn
  Socket.forkPingThread socket 10
  env <- mkWebSocketEnv pg logCfg
  _ <- async . forever $ do
    subsBS <- Socket.receiveData socket
    runWebSocketM env $ clientMsgHandler subsBS
  runConduit
    (transPipe (runWebSocketM env) msgConduit .| socketSink socket)
  where
    socketSink
      :: Socket.Connection
      -> ConduitT WebSocketMsg Void IO ()
    socketSink socket = awaitForever $ \msg ->
      let payload = cs @_ @Text . A.encode $ msg
      in lift $ Socket.sendTextData socket payload


defaultWSApplet :: WSApplet
defaultWSApplet =
  let clientH subsBS = case A.eitherDecode $ cs subsBS of
        Left err ->
          K.logFM K.ErrorS (fromString $ "Failed to decode subscription message! " <> err)
        Right msg -> case msg of
          Update subsUpdate ->
            case mkSubscriptionKey subsUpdate of
              Nothing -> K.logFM K.ErrorS "Failed to make subscription key from update!"
              Just s@(SubscriptionID sid,_,_) -> do
                K.logFM K.DebugS (fromString $ "Received subscription update with SubscriptionID " <> sid)
                WebSocketEnv{subscriptionRef} <- ask
                liftIO $ modifyIORef subscriptionRef (s : )
          Cancel subsID@(SubscriptionID sid) -> do
            K.logFM K.DebugS (fromString $ "Cancelling SubscriptionID " <> sid)
            WebSocketEnv{subscriptionRef} <- ask
            let p (_subsID, _, _) = subsID /= _subsID
            liftIO $ modifyIORef subscriptionRef $ filter p
      msgC = listenerC .| rawChangeC .| subscriptionC
  in WSApplet
       { clientMsgHandler = clientH
       , msgConduit = msgC
       }

listenerC
  :: MonadPG m
  => ConduitT () EventID m ()
listenerC = do
  _ <- lift $ runDB $ \conn -> PG.execute_ conn "LISTEN raw_change_channel"
  forever $ do
    notification <- lift $ runDB $ \conn -> PG.getNotification conn
    let channel = cs $ PG.notificationChannel notification
        _data = PG.notificationData notification
    lift $ K.logFM K.InfoS (fromString $ "Received notification on channel " <> channel <> " -- " <> cs _data)
    let eeid =  fmap EventID . parseHexString $ cs _data :: Either String EventID
    case eeid of
      Left e ->
        let msg = "ParseError in channel " <> channel <> ", " <> "couldn't parse as EventID: " <> show e
        in lift (K.logFM K.ErrorS $ fromString msg) *> error msg
      Right eid -> yield eid


rawChangeC :: (MonadThrow m, MonadPG m) => ConduitT EventID RC.RawChange m ()
rawChangeC = awaitForever $ \eid -> lift $ getRawChange eid
  where
    getRawChange :: (MonadThrow m, MonadPG m) => EventID -> m RC.RawChange
    getRawChange eid = queryExact $ \conn -> O.runQuery conn q
      where
        q = proc () -> do
          rc <- O.queryTable RC.rawChangeTable -< ()
          O.restrict -< RC.eventID rc  O..== O.constant eid
          returnA -< rc

subscriptionC
  :: ( MonadPG m
     , MonadReader WebSocketEnv m
     )
  => ConduitT RC.RawChange WebSocketMsg m ()
subscriptionC = awaitForever $ \rc -> do
  WebSocketEnv{subscriptionRef} <- lift ask
  ks <- lift . liftIO $ readIORef subscriptionRef
  let matchingSubscriptions = filterSubscriptionKeys rc ks
      outgoingMessages = map (mkWSMessage rc) matchingSubscriptions
  sourceList outgoingMessages
  where

    filterSubscriptionKeys
      :: RC.RawChange
      -> [SubscriptionKey]
      -> [SubscriptionKey]
    filterSubscriptionKeys RC.RawChange{address, topics} ks =
      let primaryTopic = case topics of
            t : _ -> t
            _     -> error "expected topics to contain at least one item!"
          p (_, _addresses, _topics) = address `L.elem` _addresses && primaryTopic `elem` _topics
      in filter p ks

    mkWSMessage
      :: RC.RawChange
      -> SubscriptionKey
      -> WebSocketMsg
    mkWSMessage rc (_id,_,_) = WebSocketMsg _id rc
