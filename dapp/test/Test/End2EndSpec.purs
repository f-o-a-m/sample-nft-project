module Test.End2EndSpec where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (fromJust)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Test.Actions (faucetTokens)
import Test.End2EndConfig (Contracts(..), End2EndConfig)
import Test.Spec (SpecT, beforeAll_, describe, it, parallel)
import Test.Utils (TestEnv, go, mkSignalAttrGen)

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
    describe "indexer/server interaction" $ parallel do
      it "can track foam token transfers" do
        pure unit
      it "can track signal token creation" do
        pure unit
      it "can track signals for sale" do
        pure unit
      it "can track signals sold" do
        pure unit

getFoamTokenTransfers settings = do
  let baseUrl = settings.baseUrl
  res <- AX.request (AX.defaultRequest { url = baseUrl <> "/foam_token/transfers"
                                       , method = Left GET
                                       , responseFormat = ResponseFormat.json
                                       }
                    )
  pure res.body
