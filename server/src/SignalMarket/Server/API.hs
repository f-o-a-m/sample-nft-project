module SignalMarket.Server.API
 ( API
 , api
 , FoamTokenAPI
 , SignalTokenAPI
 , ConfigAPI
 ) where

import           Data.Proxy
import           Servant
import           SignalMarket.Common.EventTypes                     (EthAddress,
                                                                     HexString,
                                                                     TokenID)
import qualified SignalMarket.Common.Models.FoamTokenTransfer       as FoamTokenTransfer
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as SignalTokenTrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer     as SignalTokenTransfer
import           SignalMarket.Server.API.Types
import           SignalMarket.Server.Config                         (Contracts)

type API =
       ConfigAPI
  :<|> FoamTokenAPI
  :<|> SignalTokenAPI

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

type SignalTokenAPI =
     "signal"
  :> (GetSignalTokenTransfers :<|> GetSignalTokenByID :<|> GetSignalTokenByCST)

type GetSignalTokenTransfers =
      "transfers"
  :> QueryParam "to" EthAddress
  :> QueryParam "from" EthAddress
  :> QueryParam "token_id" TokenID
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> QueryParam "ordering" BlockNumberOrdering
  :> Get '[JSON] [WithMetadata SignalTokenTransfer.Transfer]

type GetSignalTokenByID =
      "id"
  :> Capture "token_id" TokenID
  :> Get '[JSON] (WithMetadata SignalTokenTrackedToken.TrackedToken)

type GetSignalTokenByCST =
      "cst"
  :> Capture "cst" HexString
  :> Get '[JSON] (WithMetadata SignalTokenTrackedToken.TrackedToken)
