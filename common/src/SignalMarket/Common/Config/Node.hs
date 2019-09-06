module SignalMarket.Common.Config.Node where

import           Control.Error
import           Control.Monad.Error.Class     (throwError)
import           Data.Text                     (Text)
import           Network.Ethereum.Api.Net      (version)
import           Network.Ethereum.Api.Provider (Provider, runWeb3With)
import           Network.HTTP.Client           (Manager)

getNetworkID
  :: Manager
  -> Provider
  -> ExceptT String IO Text
getNetworkID manager provider = do
  eid <- runWeb3With manager provider version
  case eid of
    Left _  -> throwError "Error in getting NetworkID"
    Right a -> pure a
