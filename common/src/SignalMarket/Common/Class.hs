module SignalMarket.Common.Class where

import           Control.Exception             (Exception (..), toException)
import           Control.Monad.Catch           (MonadThrow (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import qualified Data.Aeson                    as A
import           Data.String                   (fromString)
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Database.PostgreSQL.Simple    (SqlError (..))
import qualified Database.PostgreSQL.Simple    as PG
import qualified Katip                         as K
import           Network.Ethereum.Api.Provider (Web3, Web3Error (..))

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

data SqlQueryException = SqlQueryException deriving (Eq, Show)

instance Exception SqlQueryException

queryMaybe
  :: ( MonadPG m
     , MonadThrow m
     )
  => (PG.Connection -> IO [a])
  -> m (Maybe a)
queryMaybe q = do
  res <- runDB q
  case res of
    [] -> pure $ Nothing
    [a] -> pure $ Just a
    as -> do
      K.logFM K.ErrorS $ fromString $
        "Expected at most 1 result, got " <> show (length as) <> "!"
      throwM SqlQueryException

queryExact
  :: ( MonadPG m
     , MonadThrow m
     )
  => (PG.Connection -> IO [a])
  -> m a
queryExact q = do
  mRes <- queryMaybe q
  case mRes of
    Nothing -> do
      K.logFM K.ErrorS $ fromString $
        "Expected exactly 1 result, got 0!"
      throwM SqlQueryException
    Just a -> pure a


data Web3ErrorCTX = Web3ErrorCTX Web3Error

instance A.ToJSON Web3ErrorCTX where
  toJSON (Web3ErrorCTX e) =
    let (tag :: String, message) = case e of
            JsonRpcFail msg -> ("JsonRpcFail", msg)
            ParserFail msg  -> ("ParserFail", msg)
            UserFail msg    -> ("UserFail", msg)
    in A.object [ "type" A..= tag
                , "message" A..= message
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
