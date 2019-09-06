module Test.SignalMarketSpec (spec) where

import Prelude

import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (TestConfig, takeEvent)
import Contracts.FoamToken as FoamToken
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse_)
import Data.Array ((!!))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Deploy.Main (SignalMarket, SignalToken)
import Deploy.Utils (awaitTxSuccess)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Web3 (Address, BytesN, ChainCursor(..), Ether, HexString, Provider, UIntN, Value, Web3, _from, _gas, _to, _value, convert, defaultTransactionOptions, embed, eventFilter, mkAddress, mkValue, unUIntN)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_sendTransaction)
import Network.Ethereum.Web3.Solidity.Sizes (S256, S32, s256, s32)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, before, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (assertStorageCall, assertWeb3, go, joinWeb3Fork, mkBytesGen, mkBytesN, mkUIntN, mkUIntsGen, monitorUntil, unsafeFromJust)
import Type.Proxy (Proxy(..))

type FilterEnv m =
  { logger :: String -> m Unit -- @NOTE: use this for proper parallel logging
  , uIntGen :: Int -> m (Array (UIntN S256))
  , byteGen :: Int -> m (Array (BytesN S32))
  }

type SignalMarketTestCfg r =
  ( foamToken :: DeployReceipt NoArgs
  , signalToken :: DeployReceipt SignalToken
  , tokenFaucet :: Address
  , signalMarket :: DeployReceipt SignalMarket | r
  )

spec
  :: forall r.
     TestConfig (SignalMarketTestCfg r)
  -> SpecT Aff Unit Aff Unit
spec cfg = do
  uIntV <- liftEffect $ EAVar.new 1
  byteV <- liftEffect $ EAVar.new 10
  let uIntGen = mkUIntsGen uIntV
      byteGen = mkBytesGen byteV
      env = { logger: \s -> ask >>= \logger -> liftAff $ logger s
            , uIntGen
            , byteGen
            }
  go $ spec' cfg env

spec'
  :: forall r m.
     MonadAff m
  => MonadError Error m
  => TestConfig (SignalMarketTestCfg r)
  -> FilterEnv m
  -> SpecT m Unit Aff Unit
spec' testCfg env@{ logger, uIntGen, byteGen } = do
  -- for generating signal attributes
  let zeroAddr = unsafeFromJust "Must be valid Address 000..." $
                 mkAddress =<< mkHexString "0x0000000000000000000000000000000000000000"
      { provider
      , accounts
      , foamToken: { deployAddress: foamToken }
      , signalToken: { deployAddress: signalToken }
      , signalMarket: { deployAddress: signalMarket }
      , tokenFaucet
      } = testCfg
      -- set up 2 accounts
      account1 = unsafePartial $ fromJust $ accounts !! 1
      account2 = unsafePartial $ fromJust $ accounts !! 2
  beforeAll_ (faucetTokens env { foamToken, tokenFaucet, provider, account1, account2 }) $ do
    describe "preliminary set up" $ parallel do
      it "can run the faucet" \_ -> do
        let txOpts = defaultTransactionOptions # _to ?~ foamToken
        a1balance <- assertStorageCall provider $
                     FoamToken.balanceOf txOpts Latest { _owner: account1 }
        a2balance <- assertStorageCall provider $
                     FoamToken.balanceOf txOpts Latest { _owner: account2 }
        liftAff do
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
        liftAff do
          foamTokenAddr `shouldEqual` foamToken
          signalTokenAddr `shouldEqual` signalTokenAddr
        logger $ "Signal Market is deployed."
    describe "interact with signal market" $ parallel do
      it "can mint a signal token (ERC-721)" do
        -- approval process
        -- @NOTE: `gas` is set to max
        let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                               # _from ?~ account1
                                               # _gas ?~ embed 8000000
            approvalAmount = mkUIntN s256 100
            approveAction = FoamToken.approve txOpts { _spender: signalToken
                                                     , _value: approvalAmount
                                                     }
        Tuple _ approval@(FoamToken.Approval a) <- assertWeb3 provider $
          takeEvent (Proxy :: Proxy FoamToken.Approval) foamToken approveAction
        logger $ "Approved FOAM tokens " <> show approval
        liftAff $ a.value `shouldEqual` approvalAmount
        -- minting process
        let geohash = mkBytesN s32 "420"
            radius = mkUIntN s256 10
            stake = mkUIntN s256 1
            owner = account1
            mintAction = SignalToken.mintSignal (txOpts # _to ?~ signalToken)
                                                { owner, stake, geohash, radius }
        -- SignalToken.Transfer
        -- @NOTE: `TrackedToken` is the more standard event identifier for a SignalToken
        -- we use `Transfer` here to verify ownership details and token origin
        -- @TODO: figure out how to get both the `Transfer` and `TrackedToken` event
        Tuple _ minted@(SignalToken.Transfer m) <- assertWeb3 provider $
          takeEvent (Proxy :: Proxy SignalToken.Transfer) signalToken mintAction
        logger $ "Signal token minted " <> show minted
        liftAff do
          -- verify ownership/transfer
          m._to `shouldEqual` account1
          -- a newly minted signal is always from the `zeroAddr`
          m._from `shouldEqual` zeroAddr

      let trackMintArgs = { foamToken, signalToken, provider, account1 }
      before (trackMintSignal env trackMintArgs) $ do
        it "can mark signal tokens for sale" \{ trackMint } -> do
          -- check price and Id value
          let s = unwrap trackMint
              txOpts = defaultTransactionOptions # _from ?~ account1
                                                 # _gas ?~ embed 8000000
              _tokenId = s.tokenID
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
          logger $ "Signal approved for SignalMarket " <> show signalApproval
          -- mark signal as for sale
          Tuple _ sale@(SignalMarket.SignalForSale sfs) <- assertWeb3 provider $
            takeEvent (Proxy :: Proxy SignalMarket.SignalForSale) signalMarket $ forSaleAction
          logger $ "Signal for sale " <> show sale
          liftAff do
            sfs.price `shouldEqual` _price
            sfs.signalId `shouldEqual` _tokenId

      let originalPrice = mkUIntN s256 1
      before (do
          { trackMint } <- trackMintSignal env trackMintArgs
          markSignalForSale env { trackMint, signalToken, signalMarket, provider, account1 }
        ) $ do
        it "can buy signal tokens" \{ signalForSale } -> do
          let txOpts = defaultTransactionOptions # _from ?~ account2
                                                 # _gas ?~ embed 8000000
              signal = unwrap signalForSale
              _tokenId = signal.signalId
              acc2BuyAction =
                SignalMarket.buy (txOpts # _to ?~ signalMarket
                                         # _value ?~ convert (mkValue one :: Value Ether)) { _tokenId }
          -- @NOTE: this is failing
          logger $ "Attempting to buy Signal with Id " <> show _tokenId
          -- make account2 buy the signal from account1
          Tuple _ sold@(SignalMarket.SignalSold purchase) <- assertWeb3 provider $
            takeEvent (Proxy :: Proxy SignalMarket.SignalSold) signalMarket $ acc2BuyAction
          logger $ "Signal purchased " <> show sold
          -- check sale details and transfer of ownership
          liftAff do
            purchase.signalId `shouldEqual` signal.signalId
            purchase.price `shouldEqual` originalPrice
            purchase.owner `shouldEqual` account1
            purchase.newOwner `shouldEqual` account2

      before (do
          { trackMint } <- trackMintSignal env trackMintArgs
          markSignalForSale env { trackMint, signalToken, signalMarket, provider, account1 }
        ) $ do
        it "can unlist signal tokens" \{ signalForSale } -> do
          let txOpts = defaultTransactionOptions # _gas ?~ embed 8000000
              txOpts1 = txOpts # _from ?~ account1
              txOpts2 = txOpts # _from ?~ account2
              signalId = (unwrap signalForSale).signalId
              -- only account1 can unlist the token
              unlistAction _tokenId =
                SignalMarket.unlist (txOpts1 # _to ?~ signalMarket) { _tokenId }
              acc2BuyAction _tokenId =
                SignalMarket.buy (txOpts # _to ?~ signalMarket
                                         # _value ?~ convert (mkValue one :: Value Ether)) { _tokenId }
          -- unlist the signal with account1
          Tuple _ unlisted@(SignalMarket.SignalUnlisted nfs) <- assertWeb3 provider $
            takeEvent (Proxy :: Proxy SignalMarket.SignalUnlisted) signalMarket $ unlistAction signalId
          logger $ "Signal unlisted " <> show unlisted
          liftAff $ nfs.signalId `shouldEqual` signalId
          -- trying to buy an unlisted signal should result in failure
          -- Tuple e _ <- assertWeb3 provider $
          --   takeEvent (Proxy :: Proxy SignalMarket.SignalSold) signalMarket $ acc2BuyAction signalId
          -- pure unit

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
  let txOpts =
        defaultTransactionOptions # _to ?~ recipient
                                  # _value ?~ convert (mkValue one :: Value Ether)
                                  # _from ?~ tokenFaucet
  in eth_sendTransaction txOpts

-- faucet action
-- * faucets tokens to 2 accounts
-- * faucets a single ETH to the second account
faucetTokens
  :: forall m.
     MonadAff m
  => FilterEnv m
  -> { account1 :: Address
     , account2 :: Address
     , foamToken :: Address
     , provider :: Provider
     , tokenFaucet :: Address
     }
  -> m Unit
faucetTokens { logger } { foamToken, tokenFaucet, provider, account1, account2 } = do
  -- give FOAM tokens to each of them (via faucet)
  liftAff $ flip parTraverse_ [account1, account2] \recipient -> do
    txHash <- assertWeb3 provider $ faucet { recipient, foamToken, tokenFaucet }
    awaitTxSuccess txHash provider
  logger $ "Faucetted FOAM tokens."
  -- give one ETH to account2
  txHash <- assertWeb3 provider $ ethFaucetOne { recipient: account2
                                               , tokenFaucet
                                               }
  logger $ "Faucetted ETH to account2 " <> show txHash
  awaitTxSuccess txHash provider

-- @NOTE: `TrackedToken` is used here for making a filter on specific signal attributes
trackMintSignal
  :: forall m.
     MonadAff m
  => FilterEnv m
  -> { account1 :: Address
     , provider :: Provider
     , foamToken :: Address
     , signalToken :: Address
     }
  -> m { trackMint :: SignalToken.TrackedToken }
trackMintSignal { logger, uIntGen, byteGen } { foamToken, signalToken, provider, account1 } = do
  let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ account1
                                         # _gas ?~ embed 8000000
      approvalAmount = mkUIntN s256 100
      approveAction = FoamToken.approve txOpts { _spender: signalToken
                                               , _value: approvalAmount
                                               }
  Tuple _ approval <- assertWeb3 provider $
    takeEvent (Proxy :: Proxy FoamToken.Approval) foamToken approveAction
  logger $ "Approved FOAM tokens " <> show approval
  uIntVals <- uIntGen 1
  logger $ "Made uInts values " <> show uIntVals
  byteVals <- byteGen 1
  logger $ "Made bytes values " <> show byteVals
  let geohash = unsafePartial $ fromJust $ byteVals !! 0
      radius = unsafePartial $ fromJust $ uIntVals !! 0
      stake = mkUIntN s256 1
      owner = account1
      mintAction = SignalToken.mintSignal (txOpts # _to ?~ signalToken)
                                          { owner, stake, geohash, radius }
      trackedTokenFilter = eventFilter (Proxy :: Proxy SignalToken.TrackedToken) signalToken
  fiber <- monitorUntil provider logger trackedTokenFilter (\(SignalToken.TrackedToken s) ->
                                                             s.radius == radius)
  start <- assertWeb3 provider eth_blockNumber
  txHash <- assertWeb3 provider mintAction
  awaitTxSuccess txHash provider
  {valueV} <- joinWeb3Fork fiber
  trackMint <- liftAff $ AVar.take valueV
  logger $ "Signal token minted " <> show trackMint
  pure { trackMint }

markSignalForSale
  :: forall m.
     MonadAff m
  => FilterEnv m
  -> { account1 :: Address
     , provider :: Provider
     , trackMint :: SignalToken.TrackedToken
     , signalMarket :: Address
     , signalToken :: Address
     }
  -> m { trackMint :: SignalToken.TrackedToken
       , signalForSale :: SignalMarket.SignalForSale
       }
markSignalForSale { logger } { signalToken, signalMarket, trackMint, provider, account1 } = do
  -- marking for sale
  let txOpts = defaultTransactionOptions # _from ?~ account1
                                         # _gas ?~ embed 8000000
      s = unwrap trackMint
      _tokenId = s.tokenID
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
  logger $ "Signal approved for SignalMarket " <> show signalApproval
  -- mark signal as for sale
  Tuple _ signalForSale <- assertWeb3 provider $
    takeEvent (Proxy :: Proxy SignalMarket.SignalForSale) signalMarket $ forSaleAction
  logger $ "Signal for sale " <> show signalForSale
  pure { trackMint, signalForSale }
