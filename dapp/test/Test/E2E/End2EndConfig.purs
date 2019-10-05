module Test.E2E.End2EndConfig where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), either)
import Data.EitherR (fmapL)
import Data.HTTP.Method (Method(..))
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Network.Ethereum.Web3 (Address, Provider, httpProvider)
import Network.Ethereum.Web3.Api (eth_getAccounts, net_version)
import Node.Process as NP
import Test.Utils (expectWeb3)

data Protocol = HTTP | HTTPS

newtype BaseURL = BaseURL String

type ApiSettings = { protocol :: Protocol, baseURL :: BaseURL }

newtype Contracts = Contracts
  { foamToken :: Address
  , signalToken :: Address
  , signalMarket :: Address
  }

instance showContracts :: Show Contracts where
  show (Contracts c) = show c

instance decodeJsonContracts :: DecodeJson Contracts where
  decodeJson j = do
    obj <- decodeJson j
    foamTokenReceipt <- obj .: "foamToken"
    foamToken <- foamTokenReceipt .: "address"

    signalTokenReceipt <- obj .: "signalToken"
    signalToken <- signalTokenReceipt .: "address"

    signalMarketReceipt <- obj .: "signalMarket"
    signalMarket <- signalMarketReceipt .: "address"

    pure $ Contracts { foamToken, signalToken, signalMarket }

newtype NetworkId = NetworkId String

type End2EndConfig =
  { provider :: Provider
  , apiSettings :: ApiSettings
  , contractAddresses :: Contracts
  , networkId :: NetworkId
  , accounts :: Array Address
  }

getContractAddresses :: BaseURL -> Aff Contracts
getContractAddresses (BaseURL baseUrl) = do
  res <- AX.request (AX.defaultRequest { url = baseUrl <> "/config/contracts"
                                       , method = Left GET
                                       , responseFormat = ResponseFormat.json
                                       }
                    )
  eRes <- pure $ do
    body <- fmapL (const $ "Couldn't load /config/contracts") res.body
    decodeJson body
  either (throwError <<< error <<< (append "Couldn't load contracts: ")) pure eRes

mkEnd2EndConfig :: Aff End2EndConfig
mkEnd2EndConfig = do
  provider <- liftEffect do
    url <- fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
    httpProvider url
  baseURL <- liftEffect $ fromMaybe "http://localhost:9000" <$> NP.lookupEnv "BASE_URL"
  log $ "Using BASE URL: " <> show baseURL
  accounts <- liftAff $ expectWeb3 "eth_getAccounts" provider eth_getAccounts
  nId <- liftAff $ expectWeb3 "net_version" provider net_version
  let apiSettings = { protocol: HTTP
                    , baseURL: BaseURL baseURL
                    }
      networkId = NetworkId nId
  contractAddresses <- getContractAddresses apiSettings.baseURL
  log $ "Using addresses: " <> show contractAddresses
  pure { provider, apiSettings, contractAddresses, networkId, accounts }
