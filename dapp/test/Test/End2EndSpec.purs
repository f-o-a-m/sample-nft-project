module Test.End2EndSpec where

import Prelude

import Contracts.FoamToken as FoamToken
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array ((!!))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Network.Ethereum.Web3 (ChainCursor(..), _to, defaultTransactionOptions, unUIntN)
import Partial.Unsafe (unsafePartial)
import Test.Actions (faucetTokens)
import Test.End2EndConfig (Contracts(..), End2EndConfig)
import Test.Spec (SpecT, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Utils (TestEnv, assertStorageCall, go, mkSignalAttrGen)

spec :: End2EndConfig -> SpecT Aff Unit Aff Unit
spec cfg = do
  uIntV <- liftEffect $ EAVar.new 10
  let signalAttrGen = mkSignalAttrGen uIntV
      env = { logger: \s -> ask >>= \logger -> liftAff $ logger s
            , signalAttrGen
            }
  go $ spec' cfg env

spec'
  :: forall m.
     MonadAff m
  => MonadError Error m
  => End2EndConfig
  -> TestEnv m
  -> SpecT m Unit Aff Unit
spec' cfg env@{ logger, signalAttrGen } = do
  let { provider
      , apiSettings
      , contractAddresses
      , networkId
      , accounts
      } = cfg
      Contracts { foamToken
                , signalToken
                , signalMarket
                } = contractAddresses
      tokenFaucet = unsafePartial $ fromJust $ accounts !! 0
      account1 = unsafePartial $ fromJust $ accounts !! 1
      account2 = unsafePartial $ fromJust $ accounts !! 2
  beforeAll_ (faucetTokens env { foamToken, tokenFaucet, provider, account1, account2 }) $ do
    describe "preliminary set up" $ parallel do
      it "can run the faucet" \_ -> do
        let txOpts = defaultTransactionOptions # _to ?~ foamToken
        a1balance <- assertStorageCall provider $
                     FoamToken.balanceOf txOpts Latest { account: account1 }
        a2balance <- assertStorageCall provider $
                     FoamToken.balanceOf txOpts Latest { account: account2 }
        liftAff do
          unUIntN a1balance `shouldSatisfy` (_ > zero)
          unUIntN a2balance `shouldSatisfy` (_ > zero)
