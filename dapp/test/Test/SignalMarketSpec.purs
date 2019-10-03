module Test.SignalMarketSpec (spec) where

import Prelude

import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (TestConfig)
import Contracts.FoamToken as FoamToken
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse_)
import Data.Array ((!!))
import Data.Either (isLeft)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Deploy.Main (SignalMarket, SignalToken)
import Deploy.Utils (awaitTxSuccess, safeSignalToSale)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Web3 (Address, BytesN, ChainCursor(..), Ether, HexString, Provider, UIntN, Value, Web3, _from, _gas, _to, _value, convert, defaultTransactionOptions, embed, eventFilter, mkAddress, mkValue, unUIntN)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Solidity.Sizes (S256, S32, s256)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, before, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (assertStorageCall, assertWeb3, go, mkSignalAttrGen, mkUIntN, monitorUntil, unsafeFromJust)
import Type.Proxy (Proxy(..))

type MarketEnv m =
  { logger :: String -> m Unit -- @NOTE: use this for proper parallel logging
  , signalAttrGen :: m { geohash :: BytesN S32
                       , radius :: UIntN S256
                       }
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
  uIntV <- liftEffect $ EAVar.new 10
  let signalAttrGen = mkSignalAttrGen uIntV
      env = { logger: \s -> ask >>= \logger -> liftAff $ logger s
            , signalAttrGen
            }
  go $ spec' cfg env

spec'
  :: forall r m.
     MonadAff m
  => MonadError Error m
  => TestConfig (SignalMarketTestCfg r)
  -> MarketEnv m
  -> SpecT m Unit Aff Unit
spec' testCfg env@{ logger, signalAttrGen } = do
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
                     FoamToken.balanceOf txOpts Latest { account: account1 }
        a2balance <- assertStorageCall provider $
                     FoamToken.balanceOf txOpts Latest { account: account2 }
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
      it "can verify trackMintSignal is working" do
        attrs@{ geohash, radius } <- signalAttrGen
        logger $ "INIT: Attempting to make a signal with attrs " <> show attrs
        { trackMint } <- trackMintSignal env { geohash, radius, foamToken, signalToken, provider, account1 }
        -- @NOTE: only radius works; geohash is weird
        let s = unwrap trackMint
        s.radius `shouldEqual` radius

    describe "interact with signal market" $ parallel do
      before (do
          { geohash, radius } <- signalAttrGen
          trackMintSignal env { geohash, radius, foamToken, signalToken, provider, account1 }
        ) $ do
        it "can mark signal tokens for sale" \{ trackMint } -> do
          -- check price and Id value
          let s = unwrap trackMint
              txOpts = defaultTransactionOptions # _from ?~ account1
                                                 # _gas ?~ embed 8000000
              _tokenId = s.tokenID
              _price = mkUIntN s256 1 -- this is ETH price
              signalApproveAction =
                SignalToken.approve (txOpts # _to ?~ signalToken) { to: signalMarket
                                                                  , tokenId: _tokenId
                                                                  }
              signalApproveFilter = eventFilter (Proxy :: Proxy SignalToken.Approval) signalToken
              forSaleAction =
                SignalMarket.forSale (txOpts # _to ?~ signalMarket) { _tokenId
                                                                    , _price
                                                                    }
              forSaleFilter = eventFilter (Proxy :: Proxy SignalMarket.SignalForSale) signalMarket
          -- approve minted signal
          signalApproval <- monitorUntil provider logger signalApproveAction signalApproveFilter
          logger $ "MARK: Signal approved for SignalMarket " <> show signalApproval
          -- mark signal as for sale
          signalForSale@(SignalMarket.SignalForSale sfs) <- monitorUntil provider logger forSaleAction forSaleFilter
          logger $ "MARK: Signal for sale " <> show signalForSale
          liftAff do
            sfs.seller `shouldEqual` account1
            sfs.price `shouldEqual` _price
            sfs.tokenId `shouldEqual` _tokenId

      let originalPrice = mkUIntN s256 1
      before (do
          { geohash, radius } <- signalAttrGen
          { trackMint } <- trackMintSignal env { geohash, radius, foamToken, signalToken, provider, account1 }
          markSignalForSale env { trackMint, signalToken, signalMarket, provider, account1 }
        ) $ do
        it "can buy signal tokens" \{ signalForSale } -> do
          let txOpts = defaultTransactionOptions # _from ?~ account2
                                                 # _gas ?~ embed 8000000
              signal = unwrap signalForSale
              _saleId = signal.saleId
              acc2BuyAction =
                SignalMarket.buy (txOpts # _to ?~ signalMarket
                                         # _value ?~ convert (mkValue one :: Value Ether)) { _saleId }
              acc2BuyFilter = eventFilter (Proxy :: Proxy SignalMarket.SignalSold) signalMarket
          logger $ "BUY: Attempting to buy Signal with saleId " <> show _saleId
          -- make account2 buy the signal from account1
          sold@(SignalMarket.SignalSold purchase) <- monitorUntil provider logger acc2BuyAction acc2BuyFilter
          logger $ "BUY: Signal purchased " <> show sold
          -- check sale details and transfer of ownership
          liftAff do
            purchase.tokenId `shouldEqual` signal.tokenId
            purchase.price `shouldEqual` originalPrice
            purchase.owner `shouldEqual` account1
            purchase.newOwner `shouldEqual` account2

      before (do
          { geohash, radius } <- signalAttrGen
          { trackMint } <- trackMintSignal env { geohash, radius, foamToken, signalToken, provider, account1 }
          markSignalForSale env { trackMint, signalToken, signalMarket, provider, account1 }
        ) $ do
        it "can unlist signal tokens" \{ signalForSale } -> do
          let txOpts = defaultTransactionOptions # _gas ?~ embed 8000000
              txOpts1 = txOpts # _from ?~ account1
              s = unwrap signalForSale
              _saleId = s.saleId
              -- only account1 can unlist the token
              unlistAction = SignalMarket.unlist (txOpts1 # _to ?~ signalMarket) { _saleId }
              unlistFilter = eventFilter (Proxy :: Proxy SignalMarket.SignalUnlisted) signalMarket
          -- unlist the signal with account1
          logger $ "UNLIST: Attempting to unlist signal with saleId " <> show _saleId
          unlisted@(SignalMarket.SignalUnlisted nfs) <- monitorUntil provider logger unlistAction unlistFilter
          logger $ "UNLIST: Signal unlisted " <> show unlisted
          liftAff $ nfs.saleId `shouldEqual` _saleId
          -- attempting to buy an unlist signal, should result in an error
          -- @NOTE: `safeSignalToSale` is used here in place of the normal FFI to avoid defaulting behavior
          let signalsForSale = safeSignalToSale (txOpts # _to ?~ signalMarket) Latest s.tokenId
          tx <- assertWeb3 provider signalsForSale
          tx `shouldSatisfy` isLeft

faucet
  :: { recipient :: Address
     , foamToken :: Address
     , tokenFaucet :: Address
     }
  -> Web3 HexString
faucet { recipient, foamToken, tokenFaucet } =
  let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ tokenFaucet
  in FoamToken.transfer txOpts { recipient
                               , amount: mkUIntN s256 1000000
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
  => MarketEnv m
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
  logger $ "FAUCET: FOAM tokens"
  -- give one ETH to account2
  txHash <- assertWeb3 provider $ ethFaucetOne { recipient: account2
                                               , tokenFaucet
                                               }
  logger $ "FAUCET: ETH to account2 " <> show txHash
  awaitTxSuccess txHash provider

trackMintSignal
  :: forall m.
     MonadAff m
  => MarketEnv m
  -> { account1 :: Address
     , geohash :: BytesN S32
     , radius :: UIntN S256
     , provider :: Provider
     , foamToken :: Address
     , signalToken :: Address
     }
  -> m { trackMint :: SignalToken.TrackedToken }
trackMintSignal { logger } { geohash, radius, foamToken, signalToken, provider, account1 } = do
  let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ account1
                                         # _gas ?~ embed 8000000
      approvalAmount = mkUIntN s256 100
      approveAction = FoamToken.approve txOpts { spender: signalToken
                                               , value: approvalAmount
                                               }
      approvalFilter = eventFilter (Proxy :: Proxy FoamToken.Approval) foamToken
  approval@(FoamToken.Approval ft) <- monitorUntil provider logger approveAction approvalFilter
  logger $ "AUX: Approved FOAM tokens " <> show approval
  -- generate unique attributes
  let stake = mkUIntN s256 1
      owner = account1
      mintAction = SignalToken.mintSignal (txOpts # _to ?~ signalToken)
                                          { owner, stake, geohash, radius }
      trackedTokenFilter = eventFilter (Proxy :: Proxy SignalToken.TrackedToken) signalToken
  trackMint@(SignalToken.TrackedToken tm) <- monitorUntil provider logger mintAction trackedTokenFilter
  logger $ "AUX: Signal token minted " <> show trackMint
  pure { trackMint }

markSignalForSale
  :: forall m.
     MonadAff m
  => MarketEnv m
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
        SignalToken.approve (txOpts # _to ?~ signalToken) { to: signalMarket
                                                          , tokenId: _tokenId
                                                          }
      signalApproveFilter = eventFilter (Proxy :: Proxy SignalToken.Approval) signalToken
      forSaleAction =
        SignalMarket.forSale (txOpts # _to ?~ signalMarket) { _tokenId
                                                            , _price
                                                            }
      forSaleFilter = eventFilter (Proxy :: Proxy SignalMarket.SignalForSale) signalMarket
  -- approve minted signal
  signalApproval <- monitorUntil provider logger signalApproveAction signalApproveFilter
  logger $ "AUX: Signal approved for SignalMarket " <> show signalApproval
  -- mark signal as for sale
  signalForSale <- monitorUntil provider logger forSaleAction forSaleFilter
  logger $ "AUX: Signal for sale " <> show signalForSale
  pure { trackMint, signalForSale }
