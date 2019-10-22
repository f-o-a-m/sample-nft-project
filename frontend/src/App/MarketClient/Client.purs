module App.MarketClient.Client
  ( module Types
  , module App.MarketClient.Client
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (error)
import Servant.Client as Client
import App.MarketClient.Types (BlockNumberOrdering(..), Contracts(..), FoamTokenTransfer(..), MetaData(..), Receipt(..), WithMetaData(..)) as Types

-- ClientM
newtype ClientM a = ClientM (ReaderT Client.ClientEnv (ExceptT Client.AjaxError Aff) a)

derive newtype instance functorClientM :: Functor ClientM
derive newtype instance applyClientM :: Apply ClientM
derive newtype instance applicativeClientM :: Applicative ClientM
derive newtype instance bindClientM :: Bind ClientM
derive newtype instance monadClientM :: Monad ClientM
derive newtype instance monadEffClientM :: MonadEffect ClientM
derive newtype instance monadAffClientM :: MonadAff ClientM
derive newtype instance monadAskClientM :: MonadAsk Client.ClientEnv ClientM
derive newtype instance monadThrowClientM :: MonadThrow Client.AjaxError ClientM
derive newtype instance monadErrorClientM :: MonadError Client.AjaxError ClientM

instance runClientClientM :: Client.RunRequest ClientM where
  runRequest = Client.defaultRunRequest

runClientM
  :: Client.ClientEnv
  -> (forall a. ClientM a -> Aff (Either Client.AjaxError a))
runClientM env (ClientM m) = runReaderT m env # runExceptT

assertClientM :: Client.ClientEnv -> (forall a. ClientM a -> Aff a)
assertClientM clientEnv action = runClientM clientEnv action >>= case _ of
  Left err -> throwError (error $ Client.errorToString err)
  Right res -> pure res
