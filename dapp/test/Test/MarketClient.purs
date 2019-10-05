module Test.MarketClient where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Network.Ethereum.Web3 (HexString)
import Servant.API (type (:>))
import Servant.API as API
import Servant.Client as Client
import Test.Types (BlockNumberOrdering, Contracts, FoamTokenTransfer)

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

-- contract/config
type GetContracts =
     API.S "config"
  :> API.S "contracts"
  :> API.GET Json Contracts

getContracts :: ClientM Contracts
getContracts =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetContracts)

-- foam token transfers
newtype Address = Address HexString

instance encodeQueryParamData :: API.EncodeQueryParam Address where
  encodeQueryParam (Address a) = show a

type FoamTokenTransfers =
     API.S "foam_token"
  :> API.S "transfers"
  :> API.QPs ( to :: Maybe Address
             , from :: Maybe Address
             , limit :: Maybe Int
             , offset :: Maybe Int
             , ordering :: Maybe BlockNumberOrdering
             )
  :> API.GET Json (Array FoamTokenTransfer)

foamTokenTransfers
  :: API.QueryParams ( to :: Maybe Address
                     , from :: Maybe Address
                     , limit :: Maybe Int
                     , offset :: Maybe Int
                     , ordering :: Maybe BlockNumberOrdering
                     )
  -> ClientM (Array FoamTokenTransfer)
foamTokenTransfers =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy FoamTokenTransfers)

-- signal tokens
-- signal market for sale
-- signal market sold
