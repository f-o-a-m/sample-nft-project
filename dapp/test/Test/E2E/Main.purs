module Test.E2E.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Test.E2E.End2EndConfig as E2EConfig
import Test.E2E.Spec.End2EndSpec as E2E
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)

main :: Effect Unit
main = void <<< launchAff $ do
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
  e2eConfig <- E2EConfig.mkEnd2EndConfig
  join $ runSpecT specConfig [consoleReporter] do
    E2E.spec e2eConfig

