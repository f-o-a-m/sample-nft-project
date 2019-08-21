module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Effect.Aff (launchAff, Milliseconds(..))
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Identity (Identity(..))
import Main (deployScript)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)


import SimpleStorageSpec as SimpleStorageSpec

-- | TODO: make the options for deploy config env vars
main :: Effect Unit
main = void <<< launchAff $ do
  testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
  un Identity $ runSpecT defaultConfig {timeout = Just (Milliseconds $ 60.0 * 1000.0)} [consoleReporter] do
    SimpleStorageSpec.spec testConfig
