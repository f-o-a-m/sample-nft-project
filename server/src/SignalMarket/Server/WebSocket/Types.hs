module SignalMarket.Server.WebSocket.Types where

import           Control.Concurrent.MVar              (MVar, newMVar)
import           Control.Lens                         (lens)
import           Control.Monad.Catch                  (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Reader                 (MonadReader (..),
                                                       ReaderT, runReaderT)
import qualified Data.Aeson                           as A
import           Data.ByteString                      (ByteString)
import           Data.Conduit
import           GHC.Generics                         (Generic)
import qualified Network.WebSockets                   as Socket
import           SignalMarket.Common.Aeson            (defaultAesonOptions)
import           SignalMarket.Common.Config.Logging
import           SignalMarket.Common.EventTypes       (EthAddress, HexString)
import qualified SignalMarket.Common.Models.RawChange as RC

newtype SubscriptionID  = SubscriptionID String deriving (Eq, Show, Ord, A.FromJSON, A.ToJSON)

data Filter = Filter
  { filterAddress :: Maybe EthAddress
  , filterTopics  :: Maybe [Maybe HexString]
  } deriving Generic

instance A.FromJSON Filter where
  parseJSON = A.genericParseJSON (defaultAesonOptions "filter")

data Subscription = Subscription
  { subscriptionFilter         :: Filter
  , subscriptionSubscriptionID :: SubscriptionID
  } deriving Generic

instance A.FromJSON Subscription where
  parseJSON = A.genericParseJSON (defaultAesonOptions "subscription")

data UpdateSubscriptionMsg = Update Subscription | Cancel SubscriptionID deriving Generic

instance A.FromJSON UpdateSubscriptionMsg where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

type SubscriptionKey = (SubscriptionID, EthAddress, Maybe HexString)

data WebSocketMsg = WebSocketMsg
  { webSocketMsgSubscriptionID :: SubscriptionID
  , webSocketMsgContents       :: RC.RawChange
  } deriving (Show, Generic)

instance A.ToJSON WebSocketMsg where
  toJSON = A.genericToJSON (defaultAesonOptions "webSocketMsg")

mkSubscriptionKey
  :: Subscription
  -> Maybe SubscriptionKey
mkSubscriptionKey Subscription{..} = do
  addresses <- filterAddress subscriptionFilter
  topics <- filterTopics subscriptionFilter
  case topics of
    Just primaryTopic : _ -> Just (subscriptionSubscriptionID, addresses, Just primaryTopic)
    Nothing : _ -> Just (subscriptionSubscriptionID, addresses, Nothing)
    _ -> Nothing

data WebSocketEnv = WebSocketEnv
  { subscriptionRef :: MVar [SubscriptionKey]
  , logEnv          :: LogConfig
  }

mkWebSocketEnv
  :: LogConfig
  -> IO WebSocketEnv
mkWebSocketEnv le = do
  subs <- newMVar []
  pure $ WebSocketEnv subs le

instance HasLogConfig WebSocketEnv where
  logConfig = lens g s
    where
      g = logEnv
      s cfg lc = cfg {logEnv = lc}

newtype WebSocketM a = WebSocketM
  { _runWebSocketM :: ReaderT WebSocketEnv IO a }
  deriving (Functor, Applicative, Monad, MonadReader WebSocketEnv, MonadThrow, MonadCatch, MonadIO)

runWebSocketM :: WebSocketEnv -> forall a. WebSocketM a -> IO a
runWebSocketM env action = runReaderT (_runWebSocketM action) env

data WSApplet = WSApplet
  { clientMsgHandler :: ByteString -> WebSocketM ()
  , msgConduit       :: Socket.Connection -> ConduitT ByteString Void WebSocketM ()
  }
