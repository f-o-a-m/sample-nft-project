module SignalMarket.Server.API
 ( API
 , api
 , FoamTokenAPI
 , SignalAPI
 , SignalTokenAPI
 , SignalMarketAPI
 , ConfigAPI
 , WebSocketAPI
 ) where

import           Data.Proxy
import           Servant
import           SignalMarket.Common.EventTypes                       (EthAddress,
                                                                       SaleID,
                                                                       SaleStatus,
                                                                       TokenID)
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
  :<|> WebSocketAPI

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
  :> GetSignalWithSale


type GetSignalWithSale =
    "with_sales"
  :> QueryParam  "limit" Int
  :> QueryParam  "offset" Int
  :> QueryParams "owner" EthAddress
  :> QueryParams "token_id" TokenID
  :> Get '[JSON] SignalWithSaleResponse

--------------------------------------------------------------------------------
-- /signal/transfers
--------------------------------------------------------------------------------

type SignalTokenAPI = GetSignalTokenTransfers

type GetSignalTokenTransfers =
    "signal"
  :> "transfers"
  :> QueryParams "to" EthAddress
  :> QueryParams "from" EthAddress
  :> QueryParams "token_id" TokenID
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalTokenTransfer.Transfer]

--------------------------------------------------------------------------------
-- /signal_market
--------------------------------------------------------------------------------

type SignalMarketAPI =
  "signal_market"
    :> ( GetSignalMarketSignalForSale
    :<|> GetSignalMarketSignalSold
    :<|> GetSignalMarketHistory
    )

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
  :> QueryParams "sale_id" SaleID
  :> QueryParams "token_id" TokenID
  :> QueryParams "sold_from" EthAddress
  :> QueryParams "sold_to" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalMarketSignalSold.SignalSold]

type GetSignalMarketHistory =
     Capture "token_id" TokenID
  :> Get '[JSON] SignalWithMarketHistoryResponse

--------------------------------------------------------------------------------
type WebSocketAPI = "ws" :> Raw
--------------------------------------------------------------------------------
