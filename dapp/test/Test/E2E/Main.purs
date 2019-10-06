module Test.E2E.Main where

import Prelude

import Contracts.FoamToken (Transfer)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff)
import Network.Ethereum.Web3 as Web3
import Test.E2E.End2EndConfig (mkURL)
import Test.E2E.End2EndConfig as E2EConfig
import Test.Spec.End2EndSpec as E2E
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Test.Spec.SignalMarketSpec as SignalMarketSpec
import Test.Spec.Websocket (changeConsumer, createWebSocket, mkMonitor)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = void <<< launchAff $ do
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
  e2eConfig <- E2EConfig.mkEnd2EndConfig
  let (E2EConfig.Contracts { foamToken, signalToken, signalMarket }) = e2eConfig.contractAddresses
      tempConfig = { foamToken
                   , signalToken
                   , signalMarket
                   , accounts: e2eConfig.accounts
                   , provider: e2eConfig.provider
                   }

  ws <- createWebSocket (mkURL e2eConfig.apiSettings)
  -- TODO: remove this
  delay (Milliseconds 10000.0)
  let fltr = Web3.eventFilter (Proxy :: Proxy Transfer) foamToken
  _ <- mkMonitor ws fltr changeConsumer
  join $ runSpecT specConfig [consoleReporter] do
    SignalMarketSpec.spec tempConfig
    E2E.spec e2eConfig

