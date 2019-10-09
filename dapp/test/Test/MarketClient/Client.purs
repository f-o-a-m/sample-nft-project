module Test.MarketClient.Client
  ( module Types
  , module Test.MarketClient.Client
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (error)
import Network.Ethereum.Web3 (Address)
import Servant.API (type (:>))
import Servant.API as API
import Servant.Client as Client
import Test.MarketClient.Types (BlockNumberOrdering(..), Contracts(..), FoamTokenTransfer(..), MetaData(..), Receipt(..), WithMetaData(..)) as Types

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

-- contract/config
type GetContracts =
     API.S "config"
  :> API.S "contracts"
  :> API.GET Json Types.Contracts

getContracts :: ClientM Types.Contracts
getContracts =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetContracts)

-- foam token transfers
newtype AddressP = AddressP Address

instance encodeQueryParamData :: API.EncodeQueryParam AddressP where
  encodeQueryParam (AddressP a) = show a

type GetFoamTokenTransfers =
     API.S "foam_token"
  :> API.S "transfers"
  :> API.QPs ( to :: Array AddressP
             , from :: Array AddressP
             , limit :: Maybe Int
             , offset :: Maybe Int
             , ordering :: Maybe Types.BlockNumberOrdering
             )
  :> API.GET Json (Array (Types.WithMetaData Types.FoamTokenTransfer))

foamTokenTransfers
  :: API.QueryParams ( to :: Array AddressP
                     , from :: Array AddressP
                     , limit :: Maybe Int
                     , offset :: Maybe Int
                     , ordering :: Maybe Types.BlockNumberOrdering
                     )
  -> ClientM (Array (Types.WithMetaData Types.FoamTokenTransfer))
foamTokenTransfers =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetFoamTokenTransfers)

-- signal tokens
-- signal market for sale
-- signal market sold
