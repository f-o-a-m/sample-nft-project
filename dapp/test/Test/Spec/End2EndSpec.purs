module Test.Spec.End2EndSpec where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array (length, (!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..), fromJust)
import Effect.AVar as EAVar
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Servant.API as API
import Servant.Client (ClientEnv(..), ErrorDescription)
import Servant.Client.Error (errorDescription)
import Test.Actions (faucetTokens)
import Test.E2E.End2EndConfig (Contracts(..), End2EndConfig)
import Test.MarketClient (ClientM, foamTokenTransfers, runClientM)
import Test.Spec (SpecT, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (TestEnv, go, mkSignalAttrGen)

mkClientEnv :: Int -> ClientEnv
mkClientEnv port =
  ClientEnv { protocol: "http"
            , baseURL: "//localhost:" <> show port <> "/"
            }

runClientM'
  :: ClientEnv
  -> (forall a. ClientM a -> Aff (Either ErrorDescription a))
runClientM' env action = do
  eRes <- runClientM env action
  pure $ lmap (view errorDescription) eRes

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
      --
      clientEnv@(ClientEnv cfg) = mkClientEnv 9000
      assertClientM :: forall a. ClientM a -> Aff a
      assertClientM action = runClientM' clientEnv action >>= case _ of
        Left err -> throwError (error $ show err)
        Right res -> pure res


  beforeAll_ (faucetTokens env { foamToken, tokenFaucet, provider, account1, account2 }) $ do
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
