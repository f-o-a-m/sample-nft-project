module SignalMarket.Server.WebSocket.App where

import           Control.Concurrent.Async             (async)
import           Control.Concurrent.MVar              as MVar
import           Control.Monad                        (forM_, forever)
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (MonadReader (..))
import           Control.Monad.Trans                  (lift)
import qualified Data.Aeson                           as A
import           Data.ByteString                      (ByteString)
import           Data.Conduit
import           Data.Conduit.List                    (sourceList)
import qualified Data.List                            as L
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import qualified Database.Redis                       as Redis
import qualified Katip                                as K
import qualified Network.WebSockets                   as Socket
import           SignalMarket.Common.Config.Logging
import           SignalMarket.Common.Config.Redis
import qualified SignalMarket.Common.Models.RawChange as RC
import           SignalMarket.Server.WebSocket.Types

mkWebSocketApp
  :: Redis.Connection
  -> LogConfig
  -> WSApplet
  -> Socket.ServerApp
mkWebSocketApp redis logCfg webSocketApplet pendingConn = do
  socket <- Socket.acceptRequest pendingConn
  Socket.forkPingThread socket 10
  env <- mkWebSocketEnv logCfg
  let WSApplet{clientMsgHandler, msgConduit} = webSocketApplet
  _ <- async . forever $ do
    subsBS <- Socket.receiveData socket
    runWebSocketM env $ clientMsgHandler subsBS
  let subs =
        [ (rawChangeChannel, \bs -> runConduit $
             transPipe (runWebSocketM env) (yield bs .| msgConduit socket))
        ]
  controllers <- Redis.newPubSubController subs []
  Redis.pubSubForever redis controllers mempty

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
                liftIO $ MVar.modifyMVar_ subscriptionRef (pure . (s : ))
          Cancel subsID@(SubscriptionID sid) -> do
            K.logFM K.DebugS (fromString $ "Cancelling SubscriptionID " <> sid)
            WebSocketEnv{subscriptionRef} <- ask
            let p (_subsID, _, _) = subsID /= _subsID
            liftIO $ MVar.modifyMVar_ subscriptionRef $ pure . filter p
  in WSApplet
       { clientMsgHandler = clientH
       , msgConduit = pubsubHandler
       }

pubsubHandler
  :: ( LoggingM m
     , MonadReader WebSocketEnv m
     )
  => Socket.Connection
  -> ConduitT ByteString Void m ()
pubsubHandler conn = decodeC .| subscriptionC .| socketSink conn

decodeC
  :: LoggingM m
  => ConduitT ByteString RC.RawChange m ()
decodeC = awaitForever $ \bs -> do
  case A.eitherDecode $ cs bs of
    Right a -> yield a
    Left err -> lift $
      K.logFM K.ErrorS $ fromString $ "Error decoding pubsub message: " <> cs err

subscriptionC
  :: ( MonadReader WebSocketEnv m
     , LoggingM m
     , MonadIO m
     )
  => ConduitT RC.RawChange WebSocketMsg m ()
subscriptionC = awaitForever $ \rc -> do
  WebSocketEnv{subscriptionRef} <- lift ask
  ks <- liftIO $ MVar.readMVar subscriptionRef
  let matchingSubscriptions = filterSubscriptionKeys rc ks
      outgoingMessages = map (mkWSMessage rc) matchingSubscriptions
  forM_ outgoingMessages $ \WebSocketMsg{webSocketMsgContents} ->
    forM_ matchingSubscriptions $ \(SubscriptionID sid,_,_) -> lift $ do
      K.katipAddContext webSocketMsgContents $
        K.logFM K.DebugS $ fromString ("Outgoing WS Message -- " <> sid)
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
          p (_, _address, _topics) = address == _address && primaryTopic `L.elem` _topics
      in filter p ks

    mkWSMessage
      :: RC.RawChange
      -> SubscriptionKey
      -> WebSocketMsg
    mkWSMessage rc (_id,_,_) = WebSocketMsg _id rc

socketSink
  :: MonadIO m
  => Socket.Connection
  -> ConduitT WebSocketMsg Void m ()
socketSink socket = awaitForever $ \msg ->
  let payload = cs @_ @Text . A.encode $ msg
  in liftIO $ Socket.sendTextData socket payload
