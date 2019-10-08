module Test.Unit.Spec.SignalMarketSpec (spec) where

import Prelude

import Chanterelle.Test (TestConfig)
import Contracts.FoamToken as FoamToken
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array ((!!))
import Data.Either (isLeft)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Deploy.Utils (safeSignalToSale)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Ether, Value, _from, _gas, _to, _value, convert, defaultTransactionOptions, embed, eventFilter, mkAddress, mkValue, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafePartial)
import Test.Actions (faucetTokens, markSignalForSale, trackMintSignal)
import Test.Spec (SpecT, before, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (TestEnv, assertStorageCall, assertWeb3, go, mkSignalAttrGen, mkUIntN, monitorUntil, unsafeFromJust)
import Type.Proxy (Proxy(..))

-- this is good
-- not sure if we actually use anythiong more than the address
-- do we need the deplly hash etc? no. he we just need the hash
-- so then let's make it like contracts
type SignalMarketTestCfg r =
  ( foamToken :: Address
  , signalToken :: Address
  , signalMarket :: Address
  | r
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
  -> TestEnv m
  -> SpecT m Unit Aff Unit
spec' testCfg env@{ logger, signalAttrGen } = do
  -- for generating signal attributes
  let zeroAddr = unsafeFromJust "Must be valid Address 000..." $
                 mkAddress =<< mkHexString "0x0000000000000000000000000000000000000000"
      { provider
      , accounts
      , foamToken
      , signalToken
      , signalMarket
      } = testCfg
      -- set up 2 accounts
      account1 = unsafePartial $ fromJust $ accounts !! 1
      account2 = unsafePartial $ fromJust $ accounts !! 2
      tokenFaucet = unsafePartial $ fromJust $ accounts !! 0
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
