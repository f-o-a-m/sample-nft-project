module SignalMarket.Server.API
 ( API
 , api
 , FoamTokenAPI
 , SignalAPI
 , SignalTokenAPI
 , SignalMarketAPI
 , ConfigAPI
 ) where

import           Data.Proxy
import           Servant
import           SignalMarket.Common.EventTypes                       (EthAddress,
                                                                       SaleID,
                                                                       SaleStatus,
                                                                       TokenID,
                                                                       Value)
import qualified SignalMarket.Common.Models.FoamTokenTransfer         as FoamTokenTransfer
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as SignalMarketSignalForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold    as SignalMarketSignalSold
import qualified SignalMarket.Common.Models.SignalTokenTransfer       as SignalTokenTransfer
import           SignalMarket.Server.API.Types
import           SignalMarket.Server.Config                           (Contracts)

type API =
       ConfigAPI
  :<|> FoamTokenAPI
  :<|> SignalAPI
  :<|> SignalTokenAPI
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
  :> QueryParams "to" EthAddress
  :> QueryParams "from" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata FoamTokenTransfer.Transfer]

--------------------------------------------------------------------------------
-- /signal
--------------------------------------------------------------------------------

type SignalAPI =
     "signal_token"
  :> GetSignals


type GetSignals =
    "signal"
  :> QueryParam  "limit" Int
  :> QueryParam  "offset" Int
  :> QueryParams "owner" EthAddress
  :> QueryParams "token_id" TokenID
  :> Get '[JSON] [WithMetadata APISignal]

--------------------------------------------------------------------------------
-- /signal/transfers
--------------------------------------------------------------------------------

type SignalTokenAPI = GetSignalTokenTransfers

type GetSignalTokenTransfers =
    "signal"
  :> "transfers"
  :> QueryParam "to" EthAddress
  :> QueryParam "from" EthAddress
  :> QueryParam "token_id" TokenID
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalTokenTransfer.Transfer]

--------------------------------------------------------------------------------
-- /signal_market
--------------------------------------------------------------------------------

type SignalMarketAPI =
       "signal_market" :> GetSignalMarketSignalForSale
  :<|> "signal_market" :> GetSignalMarketSignalSold

type GetSignalMarketSignalForSale =
     "for_sale"
  :> QueryParams "sale_id" SaleID
  :> QueryParams "token_id" TokenID
  :> QueryParam "sale_status" SaleStatus
  :> QueryParams "seller" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalMarketSignalForSale.SignalForSale]

type GetSignalMarketSignalSold =
     "sold"
  :> QueryParam "sale_id" SaleID
  :> QueryParam "token_id" TokenID
  :> QueryParam "price" Value -- maybe just a normal int?
  :> QueryParam "sold_from" EthAddress
  :> QueryParam "sold_to" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalMarketSignalSold.SignalSold]

