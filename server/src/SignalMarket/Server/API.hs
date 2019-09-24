module SignalMarket.Server.API
 ( API
 , api
 , FoamTokenAPI
 , ConfigAPI
 ) where

import           Data.Proxy
import           Servant
import           SignalMarket.Common.EventTypes               (EthAddress)
import qualified SignalMarket.Common.Models.FoamTokenTransfer as FoamTokenTransfer
import           SignalMarket.Server.API.Types
import           SignalMarket.Server.Config                   (Contracts)

type API =
       ConfigAPI
  :<|> FoamTokenAPI

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
