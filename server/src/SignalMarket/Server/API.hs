module SignalMarket.Server.API
 ( API
 , api
 ) where

import Data.Proxy
import SignalMarket.Server.Config.Types (Contracts)
import Servant


type API =
  GetDeployReceipts

api :: Proxy API
api = Proxy

type GetDeployReceipts =
    "contracts" :> Get '[JSON] Contracts 