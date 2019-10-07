module Test.Unit.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Deploy.Main (deployScript)
import Effect (Effect)
import Effect.Aff (launchAff, Milliseconds(..))
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Test.Unit.Spec.SignalMarketSpec as SignalMarketSpec

-- @TODO: make the options for deploy config env vars
main :: Effect Unit
main = void <<< launchAff $ do
  testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
  traceM testConfig
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
      -- there's probably somehelper function to used here but...
      tempConfig = { foamToken:    testConfig.foamToken.deployAddress
                   , signalMarket: testConfig.signalMarket.deployAddress
                   , signalToken:  testConfig.signalToken.deployAddress
                   , provider:     testConfig.provider
                   , accounts:     testConfig.accounts
                   }
  join $ runSpecT specConfig [consoleReporter] do
    SignalMarketSpec.spec tempConfig
