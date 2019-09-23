module SignalMarket.Server.API
 ( API
 , api
 , FoamTokenAPI
 , SignalMarketAPI
 , ConfigAPI
 ) where

import           Data.Proxy
import           Servant
import           Servant.API
import           SignalMarket.Common.EventTypes                       (EthAddress,
                                                                       Value)
import qualified SignalMarket.Common.Models.FoamTokenTransfer         as FoamTokenTransfer
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as SignalMarketSignalForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold    as SignalMarketSignalSold
import           SignalMarket.Server.API.Types
import           SignalMarket.Server.Config                           (Contracts)

type API =
       ConfigAPI
  :<|> FoamTokenAPI
  :<|> SignalMarketAPI

api :: Proxy API
api = Proxy

--------------------------------------------------------------------------------
-- /config
--------------------------------------------------------------------------------

type GetDeployReceipts =
    "contracts" :> Get '[JSON] Contracts

type ConfigAPI =
    "config"
  :> GetDeployReceipts

--------------------------------------------------------------------------------
-- /foam_token
--------------------------------------------------------------------------------

type FoamTokenAPI =
     "foam_token"
  :> GetFoamTokenTransfers

type GetFoamTokenTransfers =
    "transfers"
  :> QueryParam "to" EthAddress
  :> QueryParam "from" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata FoamTokenTransfer.Transfer]

--------------------------------------------------------------------------------
-- /signal_market
--------------------------------------------------------------------------------

type SignalMarketAPI =
     "signal_market"
  :> GetSignalMarketSignalForSale
  :> GetSignalMarketSignalSold

type GetSignalMarketSignalForSale =
     "for_sale"
  :> QueryParam "sale_id" Int
  :> QueryParam "token_id" Int
  :> QueryParam "price" Value -- maybe just a normal int?
  :> QueryParam "sale_status" SignalMarketSignalForSale.HStatus
  :> QueryParam "seller" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalMarketSignalForSale.SignalForSale]

type GetSignalMarketSignalSold =
     "sold"
  :> QueryParam "sale_id" Int
  :> QueryParam "token_id" Int
  :> QueryParam "price" Value -- maybe just a normal int?
  :> QueryParam "sold_from" EthAddress
  :> QueryParam "sold_to" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalMarketSignalSold.SignalSold]
