module SignalMarketSpec (spec) where

import Prelude

import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Internal.Utils.Web3 (pollTransactionReceipt)
import Chanterelle.Test (TestConfig, assertWeb3)
import Contracts.FoamToken as FoamToken
import Control.Parallel (parTraverse_)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Main (SignalMarket)
import Network.Ethereum.Web3 (class KnownSize, Address, BytesN, CallError, ChainCursor(..), DLProxy, HexString, Provider, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, _from, _to, defaultTransactionOptions, embed, fromByteString, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafeCrashWith, unsafePartial, unsafePartialBecause)
import Test.Spec (SpecT, beforeAll_, describe, it, pending')
import Test.Spec.Assertions (shouldSatisfy)

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
                , tokenFaucet :: Address
                , signalMarket :: DeployReceipt SignalMarket | r)
  -> SpecT Aff Unit Aff Unit
spec { provider
     , accounts
     , foamToken: {deployAddress: foamToken}
     , signalMarket: {deployAddress: signalMarket}
     , tokenFaucet
     } = do
  -- beforeAll_ ?s
  -- set up 2 accounts
  -- give tokens to one of them (faucet)
  describe "interact with signal market" do
    let account1 = unsafePartial $ fromJust $ accounts !! 1
        account2 = unsafePartial $ fromJust $ accounts !! 2
    beforeAll_ (do
        flip parTraverse_ [account1, account2] \recipient -> do
          txHash <- assertWeb3 provider $ faucet { recipient, foamToken, tokenFaucet }
          awaitTxSuccess txHash provider
      ) $ do

      it "can run the faucet" do
        let txOpts = defaultTransactionOptions # _to ?~ foamToken
        a1balance <- assertStorageCall provider $ FoamToken.balanceOf txOpts Latest { _owner: account1 }
        a2balance <- assertStorageCall provider $ FoamToken.balanceOf txOpts Latest { _owner: account2 }
        unUIntN a1balance `shouldSatisfy` (_ > zero)
        unUIntN a2balance `shouldSatisfy` (_ > zero)

      -- you need to approve some tokens before this
      -- then use mint
      -- @TODO start this in a branch
      pending' "can make a signal token (ERC-721)" do
        pure unit

      pending' "can verify the signal market is deployed" do
        pure unit

      pending' "can list signal tokens for sale" do
        pure unit

      pending' "can buy signal tokens" do
        pure unit
