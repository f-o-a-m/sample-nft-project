module E2E.End2EndConfig
  ( End2EndConfig
  , ContractAddresses
  , mkEnd2EndConfig
  ) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Data.Array ((!!), drop)
import Data.Maybe (fromJust, fromMaybe)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (Address, Provider, httpProvider)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Node.Process as NP
import Partial.Unsafe (unsafePartial)
import Servant.Client (ClientEnv(..))
import Test.MarketClient.Client as API
import App.Websocket (createWebSocket, WebSocket)

type End2EndConfig =
  { provider :: Provider
  , clientEnv :: ClientEnv
  , ws :: WebSocket
  , contractAddresses :: ContractAddresses
  , accounts :: Array Address
  , faucetAddress :: Address
  }

type ContractAddresses =
  { foamToken :: Address
  , signalToken :: Address
  , signalMarket  :: Address
  }

mkEnd2EndConfig :: Aff End2EndConfig
mkEnd2EndConfig = do
  provider <- liftEffect do
    url <- fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
    httpProvider url
  clientEnv@(ClientEnv{baseURL, protocol}) <- liftEffect getClientEnv
  accounts <- liftAff $ assertWeb3 provider eth_getAccounts
  contractAddresses <- getContractAddresses clientEnv
  let apiURL = protocol <> ":" <> baseURL
  ws <- createWebSocket apiURL <* delay (Milliseconds 5000.0)
  pure { provider
       , clientEnv
       , contractAddresses
       , accounts: drop 1 accounts
       , faucetAddress: unsafePartial fromJust $ accounts !! 0
       , ws
       }
  where

  getClientEnv = do
    mbaseURL <- NP.lookupEnv "API_BASE_URL"
    let protocol = "http"
        baseURL = fromMaybe "//localhost:9000/" mbaseURL
    pure $ ClientEnv {baseURL, protocol: "http"}

  getContractAddresses clientEnv = do
    API.Contracts contracts <- API.assertClientM clientEnv API.getContracts
    let
      { foamToken: API.Receipt {address: ft}
      , signalToken: API.Receipt {address: st}
      , signalMarket: API.Receipt {address: sm}
      } = contracts
    pure { foamToken: ft
         , signalToken: st
         , signalMarket: sm
         }

