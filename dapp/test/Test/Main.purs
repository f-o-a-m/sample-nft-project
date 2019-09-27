module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Effect.Aff (launchAff, Milliseconds(..))
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Deploy.Main (deployScript)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Debug.Trace (traceM)

import Test.SignalMarketSpec as SignalMarketSpec
import Test.End2End as E2E

-- @TODO: make the options for deploy config env vars
main :: Effect Unit
main = void <<< launchAff $ do
  testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
  traceM testConfig
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
  e2e <- E2E.mkEnd2EndConfig
  join $ runSpecT specConfig [consoleReporter] do
    SignalMarketSpec.spec testConfig
