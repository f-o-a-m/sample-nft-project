module Test.SignalMarketSpec (spec) where

import Prelude

import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (TestConfig, assertWeb3, takeEvent)
import Contracts.FoamToken as FoamToken
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.Parallel (parTraverse_)
import Data.Array ((!!))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Deploy.Main (SignalMarket, SignalToken)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Ether, HexString, Provider, Value, Web3, _from, _gas, _to, _value, convert, defaultTransactionOptions, embed, mkAddress, mkValue, unUIntN)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Solidity.Sizes (s256, s32)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, before, beforeAll_, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Deploy.Utils (awaitTxSuccess)
import Test.Utils (assertStorageCall, mkBytesN, mkUIntN, unsafeFromJust)

-- import Effect.Class.Console (log)

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

ethFaucetOne
  :: { recipient :: Address
     , tokenFaucet :: Address
     }
  -> Web3 HexString
ethFaucetOne { recipient, tokenFaucet } =
  eth_sendTransaction $ defaultTransactionOptions # _to ?~ recipient
                                                  # _value ?~ convert (mkValue one :: Value Ether)
                                                  # _from ?~ tokenFaucet

-- faucet action
-- * faucets tokens to 2 accounts
-- * faucets a single ETH to the second account
faucetTokens
  :: { account1 :: Address
     , account2 :: Address
     , foamToken :: Address
     , provider :: Provider
     , tokenFaucet :: Address
     }
  -> Aff Unit
faucetTokens { foamToken, tokenFaucet, provider, account1, account2 } = do
  -- give FOAM tokens to each of them (via faucet)
  flip parTraverse_ [account1, account2] \recipient -> do
    txHash <- assertWeb3 provider $ faucet { recipient, foamToken, tokenFaucet }
    awaitTxSuccess txHash provider
  -- give one ETH to account2
  txHash <- assertWeb3 provider $ ethFaucetOne { recipient: account2
                                               , tokenFaucet
                                               }
  awaitTxSuccess txHash provider

approveAndMintSignal
  :: { account1 :: Address
     , provider :: Provider
     , foamToken :: Address
     , signalToken :: Address
     }
  -> Aff { mintTransfer :: SignalToken.Transfer }
approveAndMintSignal { foamToken, signalToken, provider, account1 } = do
  -- approval process
  -- @NOTE: `_gas` sets the max amount the user is willing to pay
  let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ account1
                                         # _gas ?~ embed 8000000
      approvalAmount = mkUIntN s256 100
      approveAction = FoamToken.approve txOpts { _spender: signalToken
                                               , _value: approvalAmount
                                               }
  Tuple _ approval <- assertWeb3 provider $
    takeEvent (Proxy :: Proxy FoamToken.Approval) foamToken approveAction

  -- minting process
  let geohash = mkBytesN s32 "420"
      radius = mkUIntN s256 10
      stake = mkUIntN s256 1
      owner = account1
      mintAction = SignalToken.mintSignal (txOpts # _to ?~ signalToken)
                                          { owner, stake, geohash, radius }
  -- SignalToken.Transfer
  -- @TODO: figure out how to get both the transfer and `TrackedToken` event
  Tuple _ mintTransfer@(SignalToken.Transfer s) <- assertWeb3 provider $
    takeEvent (Proxy :: Proxy SignalToken.Transfer) signalToken mintAction

  pure { mintTransfer }

markSignalForSale ::
  { account1 :: Address
  , provider :: Provider
  , mintTransfer :: SignalToken.Transfer
  , signalMarket :: Address
  , signalToken :: Address
  }
  -> Aff { mintTransfer :: SignalToken.Transfer
         , signalForSale :: SignalMarket.SignalForSale
         }
markSignalForSale { signalToken, signalMarket, mintTransfer, provider, account1 } = do
  -- marking for sale
  let txOpts = defaultTransactionOptions # _from ?~ account1
                                         # _gas ?~ embed 8000000
      s = unwrap mintTransfer
      _tokenId = s._tokenId
      _price = mkUIntN s256 1 -- this is ETH price
      signalApproveAction =
        SignalToken.approve (txOpts # _to ?~ signalToken) { _to: signalMarket
                                                          , _tokenId
                                                          }
      forSaleAction =
        SignalMarket.forSale (txOpts # _to ?~ signalMarket) { _tokenId
                                                            , _price
                                                            }
  -- approve minted signal
  Tuple _ signalApproval <- assertWeb3 provider $
    takeEvent (Proxy :: Proxy SignalToken.Approval) signalToken $ signalApproveAction

  -- mark signal as for sale
  Tuple _ signalForSale <- assertWeb3 provider $
    takeEvent (Proxy :: Proxy SignalMarket.SignalForSale) signalMarket $ forSaleAction

  pure { mintTransfer, signalForSale }

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
    let zeroAddr = unsafeFromJust "Must be valid Address 000..." $
                   mkAddress =<< mkHexString "0x0000000000000000000000000000000000000000"
        -- set up 2 accounts
        account1 = unsafePartial $ fromJust $ accounts !! 1
        account2 = unsafePartial $ fromJust $ accounts !! 2
    beforeAll_ (faucetTokens { foamToken, tokenFaucet, provider, account1, account2 }) $ do
      it "can run the faucet" do
        let txOpts = defaultTransactionOptions # _to ?~ foamToken
        a1balance <- assertStorageCall provider $
                     FoamToken.balanceOf txOpts Latest { _owner: account1 }
        a2balance <- assertStorageCall provider $
                     FoamToken.balanceOf txOpts Latest { _owner: account2 }
        unUIntN a1balance `shouldSatisfy` (_ > zero)
        unUIntN a2balance `shouldSatisfy` (_ > zero)
        -- @TODO (maybe): check acc2 ETH balance

      -- @NOTE: at this point all contracts are already deployed
      -- to test for a successfully deployed contract, verify that
      -- all global get functions are pointed to the correct contract addresses
      it "can verify the signal market is deployed" do
        let txOpts = defaultTransactionOptions # _to ?~ signalMarket
        -- global constructor calls
        foamTokenAddr <- assertStorageCall provider $
                         SignalMarket.foamToken txOpts Latest
        signalTokenAddr <- assertStorageCall provider $
                           SignalMarket.signalToken txOpts Latest
        foamTokenAddr `shouldEqual` foamToken
        signalTokenAddr `shouldEqual` signalTokenAddr

      let approveAndMintArgs = { foamToken, signalToken, provider, account1 }
      before (approveAndMintSignal approveAndMintArgs) $ do
        it "can mint a signal token (ERC-721)" \{ mintTransfer } -> do
          let minted = unwrap mintTransfer
          -- verify ownership/transfer
          minted._to `shouldEqual` account1
          -- a newly minted signal is always from the `zeroAddr`
          minted._from `shouldEqual` zeroAddr

      before (do
          { mintTransfer } <- approveAndMintSignal approveAndMintArgs
          markSignalForSale { mintTransfer, signalToken, signalMarket, provider, account1 }
        ) $ do
        let originalPrice = mkUIntN s256 1
        it "can mark signal tokens for sale" \{ mintTransfer, signalForSale } -> do
          -- check price and Id value
          let s = unwrap signalForSale
              m = unwrap mintTransfer
          s.price `shouldEqual` originalPrice
          s.signalId `shouldEqual` m._tokenId

        it "can buy signal tokens" \{ signalForSale } -> do
          let txOpts = defaultTransactionOptions # _from ?~ account2
                                                 # _gas ?~ embed 8000000
              signal = unwrap signalForSale
              acc2BuyAction _tokenId =
                SignalMarket.buy (txOpts # _to ?~ signalMarket
                                         # _value ?~ convert (mkValue one :: Value Ether)) { _tokenId }
          -- make account2 buy the signal from account1
          Tuple _ (SignalMarket.SignalSold purchase) <- assertWeb3 provider $
            takeEvent (Proxy :: Proxy SignalMarket.SignalSold) signalMarket $ acc2BuyAction signal.signalId
          -- check sale details and transfer of ownership
          purchase.signalId `shouldEqual` signal.signalId
          purchase.price `shouldEqual` originalPrice
          purchase.owner `shouldEqual` account1
          purchase.newOwner `shouldEqual` account2