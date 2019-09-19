module SignalMarket.Server.Application
  ( AppHandler
  , runAppHandler
  ) where

import           Control.Monad.Catch           (MonadThrow, try)
import           Control.Monad.Except          (ExceptT, MonadError)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (MonadReader, ReaderT, asks,
                                                runReaderT)
import           Network.Ethereum.Api.Provider (runWeb3With)
import           Servant.Server                (Handler (..), ServerError)
import           SignalMarket.Common.Class
import           SignalMarket.Server.Config    (AppConfig (..))

newtype AppHandler a = AppHandler
  { unAppHandler :: ReaderT AppConfig (ExceptT ServerError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadError ServerError, MonadThrow)

instance MonadPG AppHandler where
    runDB' action = do
      connection <- asks appCfgPGConnection
      liftIO . try $ action connection

instance MonadWeb3 AppHandler where
    runWeb3' action = do
      (manager, provider) <- asks appCfgWeb3Manager
      liftIO $ runWeb3With provider manager action

runAppHandler :: AppConfig -> (forall a. AppHandler a -> Handler a)
runAppHandler cfg = Handler . flip runReaderT cfg . unAppHandler
