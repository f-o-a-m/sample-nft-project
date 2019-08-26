module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Effect.Aff (launchAff, Milliseconds(..))
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Main (deployScript)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)

import SignalMarketSpec as SignalMarketSpec

-- | TODO: make the options for deploy config env vars
main :: Effect Unit
main = void <<< launchAff $ do
  testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 60.0 * 1000.0)}
  join $ runSpecT specConfig [consoleReporter] do
    SignalMarketSpec.spec testConfig
