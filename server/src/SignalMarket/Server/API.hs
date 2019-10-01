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
import           SignalMarket.Common.EventTypes                       (ByteNValue,
                                                                       EthAddress,
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
  :> QueryParam "to" EthAddress
  :> QueryParam "from" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata FoamTokenTransfer.Transfer]

--------------------------------------------------------------------------------
-- /signal
--------------------------------------------------------------------------------

type SignalAPI =
     "signal"
  :> (GetSignalByID :<|> GetSignalByCST :<|> GetSignalsByOwner :<|> GetSignalsByCreator :<|> GetSignalsByFilter)

type GetSignalByID =
      "id"
  :> Capture "token_id" TokenID
  :> Get '[JSON] APISignal

type GetSignalByCST =
      "cst"
  :> Capture "cst" ByteNValue
  :> Get '[JSON] APISignal

type GetSignalsByOwner =
    "owned-by"
  :> Capture "address" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] [APISignal]

type GetSignalsByCreator =
    "created-by"
  :> Capture "address" EthAddress
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] [APISignal]

type GetSignalsByFilter =
  "filtered"
  :> QueryParams "created-by" EthAddress
  :> QueryParams "owned-by" EthAddress
  :> QueryParams "stake-equals" Value
  :> QueryParam  "stake-less-than" Value
  :> QueryParam  "stake-less-than-or-equals" Value
  :> QueryParam  "stake-greater-than" Value
  :> QueryParam  "stake-greater-than-or-equals" Value
  :> QueryParams "radius-equals" Value
  :> QueryParam  "radius-less-than" Value
  :> QueryParam  "radius-less-than-or-equals" Value
  :> QueryParam  "radius-greater-than" Value
  :> QueryParam  "radius-greater-than-or-equals" Value
  :> QueryParam  "limit" Int
  :> QueryParam  "offset" Int
  :> Get '[JSON] [APISignal]

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
  :> QueryParam "sale_id" SaleID
  :> QueryParam "token_id" TokenID
  :> QueryParam "price" Value -- maybe just a normal int?
  :> QueryParam "sale_status" SaleStatus
  :> QueryParam "seller" EthAddress
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

