module Test.E2E.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Effect.Aff (launchAff, Milliseconds(..))
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Deploy.Main (deployScript)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Debug.Trace (traceM)

import Test.Spec.End2EndSpec as E2E
import Test.E2E.End2EndConfig as E2EConfig

-- @TODO: make the options for deploy config env vars
main :: Effect Unit
main = void <<< launchAff $ do
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
  e2eConfig <- E2EConfig.mkEnd2EndConfig
  join $ runSpecT specConfig [consoleReporter] do
    E2E.spec e2eConfig
