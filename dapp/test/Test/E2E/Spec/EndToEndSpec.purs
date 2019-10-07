module Test.E2E.Spec.End2EndSpec where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array ((!!))
import Data.Maybe (fromJust)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Test.Actions (faucetTokens)
import Test.E2E.End2EndConfig (End2EndConfig)
import Test.Spec (SpecT, beforeAll_, describe, it, parallel)
import Test.Utils (TestEnv, go, mkSignalAttrGen)


--transferFOAM
--  :: forall r.
--     { clientEnv :: ClientEnv
--     , ws :: WebSocket
--     | r
--     }
--  -> { from :: Address
--     , to :: Address
--     , value :: Int
--     }
--  -> Aff Unit
--transferFOAM {clientEnv, ws} {to, from, value} = do

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
spec' cfg@{provider, clientEnv, accounts, contractAddresses, faucetAddress} env@{logger, signalAttrGen} = do

  let {foamToken, signalToken, signalMarket} = contractAddresses
      account1 = unsafePartial $ fromJust $ accounts !! 0
      account2 = unsafePartial $ fromJust $ accounts !! 1

  beforeAll_ (faucetTokens env { foamToken,  tokenFaucet: faucetAddress, provider, account1, account2 }) $ do
    describe "indexer/server interaction" $ parallel do
      -- it "can track foam token transfers" do
      --   transfers <- liftAff $ assertClientM $ foamTokenTransfers $ API.QueryParams
      --     { from: Nothing
      --     , to: Nothing
      --     , offset: Nothing
      --     , ordering: Nothing
      --     , limit: Nothing
      --     }
      --   length transfers `shouldEqual` 0
      it "can track signal token creation" do
        pure unit
      it "can track signals for sale" do
        pure unit
      it "can track signals sold" do
        pure unit

-- getFoamTokenTransfers settings = do
--   let baseUrl = settings.baseUrl
--   res <- AX.request (AX.defaultRequest { url = baseUrl <> "/foam_token/transfers"
--                                        , method = Left GET
--                                        , responseFormat = ResponseFormat.json
--                                        }
--                     )
--   pure res.body
