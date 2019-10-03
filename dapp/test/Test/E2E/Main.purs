module Test.E2E.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Contracts.SignalMarket (signalToken)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Deploy.Main (deployScript)
import Effect (Effect)
import Effect.Aff (launchAff, Milliseconds(..))
import Test.E2E.End2EndConfig as E2EConfig
import Test.Spec.End2EndSpec as E2E
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Test.Spec.SignalMarketSpec as SignalMarketSpec

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
  join $ runSpecT specConfig [consoleReporter] do
    SignalMarketSpec.spec tempConfig
    E2E.spec e2eConfig
