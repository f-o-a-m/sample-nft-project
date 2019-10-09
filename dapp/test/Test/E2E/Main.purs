module Test.E2E.Main where

import Prelude

import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Test.E2E.End2EndConfig (mkEnd2EndConfig)
import Test.E2E.Spec.End2EndSpec as E2E
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Test.Unit.Spec.SignalMarketSpec as SignalMarketSpec

main :: Effect Unit
main = void <<< launchAff $ do
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
  e2eConfig <- mkEnd2EndConfig
  let { foamToken, signalToken, signalMarket } = e2eConfig.contractAddresses
      tempConfig = { foamToken
                   , signalToken
                   , signalMarket
                   , accounts: e2eConfig.faucetAddress `cons` e2eConfig.accounts
                   , provider: e2eConfig.provider
                   }
  join $ runSpecT specConfig [consoleReporter] do
    -- SignalMarketSpec.spec tempConfig
    E2E.spec e2eConfig

