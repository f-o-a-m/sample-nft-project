module E2E.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import E2E.End2EndConfig (mkEnd2EndConfig)
import E2E.Spec.End2EndSpec as E2E
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)

main :: Effect Unit
main = void <<< launchAff $ do
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
  e2eConfig <- mkEnd2EndConfig
  let { foamToken, signalToken, signalMarket } = e2eConfig.contractAddresses
  join $ runSpecT specConfig [consoleReporter] do
    E2E.spec e2eConfig
