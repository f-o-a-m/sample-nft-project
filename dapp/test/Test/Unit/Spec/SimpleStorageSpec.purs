module Test.Unit.Spec.SimpleStorageSpec (spec) where

import Prelude

import Chanterelle.Test (TestConfig)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Contracts.SimpleStorage as SimpleStorage
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Array ((!!))
import Data.Either (fromRight)
import Data.Lens.Setter ((.~))
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (sum)
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Web3 (EventAction(TerminateEvent), _from, _to, defaultTransactionOptions, embed, event, eventFilter, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

toNum :: forall a . Semiring a => Int -> a
toNum n = sum (replicate n one)

spec
  :: forall r .
     TestConfig (simpleStorage :: DeployReceipt NoArgs | r)
  -> Spec Unit
spec {provider, accounts, simpleStorage: {deployAddress: simpleStorage}} =
  describe "interacting with a SimpleStorage Contract" do

    it "can set the value of simple storage" $ do
      let primaryAccount = unsafePartial fromJust $ accounts !! 0
      var <- AVar.empty
      bn <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      liftEffect <<< log $ "Current blockNumber is: " <> show bn
      let n = unsafePartial $ fromJust <<< uIntNFromBigNumber s256 <<< embed $ (unsafeToInt <<< unwrap $ bn)
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just simpleStorage
      hx <- runWeb3 provider $ SimpleStorage.setCount txOptions {_count: n}
      liftEffect <<< log $ "setCount tx hash: " <> show hx

      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
      _ <- liftAff $ runWeb3 provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEffect $ log $ "Received Event: " <> show e
          _ <- liftAff $ AVar.put cs._count var
          pure TerminateEvent
      val <- AVar.take var
      Just val `shouldEqual` Just n
