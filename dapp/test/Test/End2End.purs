module Test.End2End where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), either)
import Data.EitherR (fmapL)
import Data.HTTP.Method (Method(..))
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (Address, Provider, httpProvider)
import Node.Process as NP
-- import Effect.Class.Console (log)

data Protocol = HTTP | HTTPS

newtype BaseURL = BaseURL String

type ApiSettings = { protocol :: Protocol, baseURL :: BaseURL }

-- unProtocol :: Protocol -> String
-- unProtocol HTTP = "http"
-- unProtocol HTTPS = "https"

-- mkApiURL :: ApiSettings -> String
-- mkApiURL {protocol, baseURL: (BaseURL url)} = unProtocol protocol <> ":" <> url

newtype Contracts = Contracts
  { foamToken :: Address
  , signalToken :: Address
  , signalMarket :: Address
  }
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

type End2EndConfig =
  { provider :: Provider
  , apiSettings :: ApiSettings
  , contractAddresses :: Contracts
  -- , networkId :: Foam.NetworkId
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
  -- @TODO: find out what `expectWeb3` is
  -- accountsAll <- liftAff $ expectWeb3 "eth_getAccounts" provider eth_getAccounts
  -- nId <- liftAff $ expectWeb3 "net_version" provider net_version
  let apiSettings = { protocol: HTTP
                    , baseURL: BaseURL baseURL
                    }
  contractAddresses <- getContractAddresses apiSettings.baseURL
  pure { provider, apiSettings, contractAddresses }
