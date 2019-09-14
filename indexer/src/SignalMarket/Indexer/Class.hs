module SignalMarket.Indexer.Class where

import           Control.Exception             (toException)
import           Control.Monad.Catch           (MonadThrow (..), try)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (asks)
import qualified Data.Aeson                    as A
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Database.PostgreSQL.Simple    (SqlError (..))
import qualified Database.PostgreSQL.Simple    as PG
import qualified Katip                         as K
import           Network.Ethereum.Api.Provider (Provider, Web3, Web3Error (..),
                                                runWeb3With)
import           Network.HTTP.Client           (Manager)
import           SignalMarket.Indexer.Config   (indexerCfgWeb3Manager,
                                                indexerPGConnection)
import           SignalMarket.Indexer.IndexerM

data SqlErrorCTX = SqlErrorCTX SqlError

instance A.ToJSON SqlErrorCTX where
  toJSON (SqlErrorCTX SqlError{..}) =
    let toText = cs @_ @Text
    in A.object [ "state" A..= toText sqlState
                , "exec_status" A..= show sqlExecStatus
                , "msg" A..= toText sqlErrorMsg
                , "detail" A..= toText sqlErrorDetail
                , "hint" A..= toText sqlErrorHint
                ]

instance K.ToObject SqlErrorCTX

instance K.LogItem SqlErrorCTX where
    payloadKeys _ _ = K.AllKeys

class (K.Katip m, K.KatipContext m, MonadIO m) => MonadPG m where
    runDB' :: (PG.Connection  -> IO a) -> m (Either SqlError a)
    runDB :: (PG.Connection -> IO a) -> m a

    default runDB :: MonadThrow m => (PG.Connection  -> IO a) -> m a
    runDB q = do
      eres <- runDB' q
      case eres of
        Left sqlErr -> do
          K.katipAddContext (SqlErrorCTX sqlErr) $ do
            K.logFM K.ErrorS "Postgres action caused an exception!"
            throwM (toException sqlErr)
        Right res   -> return res

instance MonadPG IndexerM where
    runDB' action = do
      connection <- asks indexerPGConnection
      liftIO . try $ action connection

data Web3ErrorCTX = Web3ErrorCTX Web3Error

instance A.ToJSON Web3ErrorCTX where
  toJSON (Web3ErrorCTX e) =
    let (tag :: String, msg) = case e of
            JsonRpcFail msg -> ("JsonRpcFail", msg)
            ParserFail msg  -> ("ParserFail", msg)
            UserFail msg    -> ("UserFail", msg)
    in A.object [ "type" A..= tag
                , "message" A..= msg
                ]

instance K.ToObject Web3ErrorCTX

instance K.LogItem Web3ErrorCTX where
    payloadKeys _ _ = K.AllKeys

class (K.Katip m, K.KatipContext m, MonadIO m) => MonadWeb3 m where
    runWeb3' :: Web3 a -> m (Either Web3Error a)
    runWeb3 :: Web3 a -> m a

    default runWeb3 :: MonadThrow m => Web3 a -> m a
    runWeb3 a = do
      eres <- runWeb3' a
      case eres of
        Left e -> do
          K.katipAddContext (Web3ErrorCTX e) $ do
            K.logFM K.ErrorS "Web3 action caused an exception!"
            throwM (toException e)
        Right res   -> return res

instance MonadWeb3 IndexerM where
    runWeb3' action = do
      (manager, provider) <- asks indexerCfgWeb3Manager
      liftIO $ runWeb3With provider manager action
