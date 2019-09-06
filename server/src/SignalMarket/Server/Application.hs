module SignalMarket.Server.Application
  ( AppHandler
  , runAppHandler
  ) where

import           Control.Monad.Except       (ExceptT, MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import           Servant.Server             (Handler (..), ServerError)
import           SignalMarket.Server.Config (AppConfig)

newtype AppHandler a = AppHandler
  { unAppHandler :: ReaderT AppConfig (ExceptT ServerError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadError ServerError)

runAppHandler :: AppConfig -> (forall a. AppHandler a -> Handler a)
runAppHandler cfg = Handler . flip runReaderT cfg . unAppHandler
