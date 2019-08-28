module SignalMarketSpec (spec) where

import Prelude

import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Internal.Utils.Web3 (pollTransactionReceipt)
import Chanterelle.Test (TestConfig, assertWeb3, takeEvent)
import Contracts.FoamToken as FoamToken
import Contracts.SignalToken as SignalToken
import Control.Parallel (parTraverse_)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Main (SignalMarket, SignalToken)
import Network.Ethereum.Web3 (class KnownSize, Address, BytesN, CallError, ChainCursor(..), DLProxy, HexString, Provider, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, _from, _gas, _to, defaultTransactionOptions, embed, fromByteString, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (s256, s32)
import Partial.Unsafe (unsafeCrashWith, unsafePartial, unsafePartialBecause)
import Test.Spec (SpecT, beforeAll_, describe, it, pending')
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))

-- @TODO: make an issue to add this to chanterelle (Chanterelle.Test module)
awaitTxSuccess :: HexString -> Provider -> Aff Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> unsafeCrashWith $ "Transaction Failed w/ hash " <> show txHash

assertStorageCall
  :: forall m a.
     MonadAff m
  => Provider
  -> Web3 (Either CallError a)
  -> m a
assertStorageCall p f = liftAff do
  eRes <- assertWeb3 p f
  case eRes of
    Right x -> pure x
    Left err -> unsafeCrashWith $ "expected Right in `assertStorageCall`, got error" <> show err

mkUIntN
  :: forall n.
     KnownSize n
  => DLProxy n
  -> Int
  -> UIntN n
mkUIntN p n = unsafePartialBecause "I know how to make a UInt" $ fromJust $ uIntNFromBigNumber p $ embed n

mkBytesN
  :: forall n.
     KnownSize n
     => DLProxy n
  -> String
  -> BytesN n
mkBytesN p s = unsafePartialBecause "I know how to make Bytes" $ fromJust $ fromByteString p =<< flip BS.fromString BS.Hex s

faucet
  :: { recipient :: Address
     , foamToken :: Address
     , tokenFaucet :: Address
     }
  -> Web3 HexString
faucet { recipient, foamToken, tokenFaucet } =
  let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ tokenFaucet
  in FoamToken.transfer txOpts { _to: recipient
                               , _value: mkUIntN s256 1000000
                               }

spec
  :: forall r .
     TestConfig ( foamToken :: DeployReceipt NoArgs
                , signalToken :: DeployReceipt SignalToken
                , tokenFaucet :: Address
                , signalMarket :: DeployReceipt SignalMarket | r)
  -> SpecT Aff Unit Aff Unit
spec { provider
     , accounts
     , foamToken: {deployAddress: foamToken}
     , signalToken: {deployAddress: signalToken}
     , signalMarket: {deployAddress: signalMarket}
     , tokenFaucet
     } = do
  describe "interact with signal market" do
    -- set up 2 accounts
    -- give tokens to each of them (via faucet)
    let account1 = unsafePartial $ fromJust $ accounts !! 1
        account2 = unsafePartial $ fromJust $ accounts !! 2
    beforeAll_ (do
        flip parTraverse_ [account1, account2] \recipient -> do
          txHash <- assertWeb3 provider $ faucet { recipient, foamToken, tokenFaucet }
          awaitTxSuccess txHash provider
      ) $ do
      liftEffect <<< log $ "acc1: " <> (show account1)
      liftEffect <<< log $ "acc2: " <> (show account2)
      it "can run the faucet" do
        let txOpts = defaultTransactionOptions # _to ?~ foamToken
        a1balance <- assertStorageCall provider $ FoamToken.balanceOf txOpts Latest { _owner: account1 }
        a2balance <- assertStorageCall provider $ FoamToken.balanceOf txOpts Latest { _owner: account2 }
        unUIntN a1balance `shouldSatisfy` (_ > zero)
        unUIntN a2balance `shouldSatisfy` (_ > zero)

      -- you need to approve some tokens before this
      -- then use mint
      -- @TODO start this in a branch
      it "can make a signal token (ERC-721)" do
        -- approval process
        let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                               # _from ?~ account1
            approvalAmount = mkUIntN s256 100
            approveAction = FoamToken.approve txOpts { _spender: signalToken
                                                     , _value: approvalAmount
                                                     }
        Tuple _ (FoamToken.Approval appr) <- assertWeb3 provider $
          takeEvent (Proxy :: Proxy FoamToken.Approval) foamToken approveAction
        appr.value `shouldEqual` approvalAmount
        -- minting process
        let geohash = mkBytesN s32 "420"
            radius = mkUIntN s256 10
            stake = mkUIntN s256 1
            owner = account1
            mintAction = SignalToken.mintSignal (txOpts # _to ?~ signalToken # _gas ?~ embed 8000000)
                                                { owner, stake, geohash, radius }
        txHash <- assertWeb3 provider $ mintAction
        liftEffect <<< log $ (show txHash)
        awaitTxSuccess txHash provider
        pure unit

      pending' "can verify the signal market is deployed" do
        pure unit

      pending' "can list signal tokens for sale" do
        pure unit

      pending' "can buy signal tokens" do
        pure unit
