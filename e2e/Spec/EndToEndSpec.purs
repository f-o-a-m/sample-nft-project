module E2E.Spec.End2EndSpec where

import Prelude

import App.API (AddressP(..), foamTokenTransfers, getSignalTokenWithHistory, getSignalTokenWithSales)
import App.Data.Radius (Radius(..))
import App.Data.Signal (Signal(..))
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.SignalId (SignalId(..))
import App.MarketClient.Client (BlockNumberOrdering(..), FoamTokenTransfer(..), MetaData(..), WithMetaData(..), assertClientM)
import App.Websocket (mkMonitor)
import Chanterelle.Test (assertWeb3)
import Contracts.FoamToken as FoamToken
import Contracts.SignalMarket as SignalMarket
import Control.Coroutine as C
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array (filter, head, (!!))
import Data.Array as Array
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Deploy.Utils (mkUIntN)
import E2E.End2EndConfig (End2EndConfig)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Network.Ethereum.Web3 (class EventFilter, Address, Change(..), Ether, HexString, Value, Web3, _from, _gas, _to, _value, convert, defaultTransactionOptions, embed, eventFilter, mkValue)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Servant.API as API
import Test.Actions (faucetTokens, markSignalForSale, trackMintSignal)
import Test.Spec (SpecT, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (TestEnv, go, mkSignalAttrGen)
import Type.Proxy (Proxy(..))



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
      it "can track foam token transfers" do
        sendTokens cfg env {to: account1, from: account2, amount: 1}
      it "can track interactions with the signal market" do
        interactSignalMarket cfg env { account1, account2 }
--      it "can get the signal owner stats via graphql api" $ void $ liftAff $ do
--        void $ getSignalOwnerStats {limit:100, offset: 0}
--        pure unit

monitorUntilEvent
  :: forall m e i ni.
     MonadAff m
  => EventFilter e
  => DecodeEvent i ni e
  => End2EndConfig
  -> Proxy e
  -> { emittingAddress :: Address
     , action :: Web3 HexString
     }
  -> m {change :: Change, event :: e}
monitorUntilEvent {ws, provider} proxy {emittingAddress, action} = liftAff do
  hashVar <- AVar.empty
  resultVar <- AVar.empty
  let eventConsumer = C.consumer \a@{change: Change {transactionHash}, event} -> do
        txHash <- AVar.read hashVar
        if txHash == transactionHash
          then Just unit <$ AVar.put a resultVar
          else pure Nothing
      fltr = eventFilter proxy emittingAddress
  canceller <- mkMonitor ws fltr eventConsumer
  hash <- assertWeb3 provider action
  AVar.put hash hashVar
  result <- AVar.take resultVar
  canceller
  pure result

sendTokens
  :: forall m.
     MonadAff m
  => End2EndConfig
  -> TestEnv m
  -> { to :: Address
     , from :: Address
     , amount :: Int
     }
  -> m Unit
sendTokens cfg@{clientEnv, contractAddresses} {logger} {to, from, amount} = do
  let {foamToken} = contractAddresses
      txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ from
      action = FoamToken.transfer txOpts { amount: mkUIntN s256 amount, recipient: to }
  logger "Submitting FoamToken transfer"
  {change: Change {transactionHash}, event} <-
     monitorUntilEvent cfg (Proxy :: Proxy FoamToken.Transfer) {action, emittingAddress: foamToken}
  let FoamToken.Transfer {from: _from, to: _to} = event
  logger "Checking FoamToken transaction from WebSockets"
  liftAff do
    from `shouldEqual` _from
    to `shouldEqual` _to
  logger "Checking FoamToken transaction from API"
  liftAff do
    let params = API.QueryParams
          { to: [AddressP to]
          , from: [AddressP from]
          , limit: Just 100
          , offset: Just 0
          , ordering: Just DESC
          }
    results <- assertClientM clientEnv $ foamTokenTransfers params
    WithMetaData {"data": FoamTokenTransfer transfer} <- findByHash transactionHash results
    from `shouldEqual` transfer.from
    to `shouldEqual` transfer.to

interactSignalMarket
  :: forall m.
     MonadAff m
  => End2EndConfig
  -> TestEnv m
  -> { account1 :: Address
     , account2 :: Address
     }
  -> m Unit
interactSignalMarket cfg@{clientEnv, provider, contractAddresses} env@{logger, signalAttrGen} {account1, account2} = do
  -- check with_sales
  let {foamToken, signalToken, signalMarket} = contractAddresses
      cursor = { limit: 100, offset: 0 }
  -- save original number of previously created signals
  sales <- liftAff $ getSignalTokenWithSales cursor
  { geohash, radius } <- signalAttrGen
  logger "Attempting to create signal with account1"
  { trackMint } <- trackMintSignal env { geohash, radius, foamToken, signalToken, provider, account1 }
  logger $ "Checking api/with_history info for minted signal " <> show trackMint
  -- check with_history
  liftAff do
    let { tokenID } = unwrap trackMint
    SignalDetails { signal: Signal s } <- getSignalTokenWithHistory (SignalId tokenID)
    s.owner `shouldEqual` account1
    s.radius `shouldEqual` Radius radius
  logger "Attempting to mark signal for sale"
  { signalForSale } <- markSignalForSale env { trackMint, signalToken, signalMarket, provider, account1 }
  logger $ "Checking api/with_history info for marked signal " <> show signalForSale
  -- check with_history
  liftAff do
    let { tokenId } = unwrap signalForSale
    SignalDetails { signal: Signal s } <- getSignalTokenWithHistory (SignalId tokenId)
    s.owner `shouldEqual` account1
    s.radius `shouldEqual` Radius radius
  -- check with_sales
  logger $ "Checking api/with_sales count"
  liftAff do
    sales' <- getSignalTokenWithSales cursor
    Array.length sales'.items `shouldSatisfy` (_ > Array.length sales.items)
  let txOpts = defaultTransactionOptions # _from ?~ account2
                                         # _gas ?~ embed 8000000
      signal = unwrap signalForSale
      _saleId = signal.saleId
      acc2BuyAction =
        SignalMarket.buy (txOpts # _to ?~ signalMarket
                                 # _value ?~ convert (mkValue one :: Value Ether)) { _saleId }
  logger "Attempting to buy signal with account2"
  {change: Change {transactionHash}, event} <-
    monitorUntilEvent cfg (Proxy :: Proxy SignalMarket.SignalSold) { action: acc2BuyAction
                                                                   , emittingAddress: signalMarket
                                                                   }
  -- check with_history
  logger $ "Checking api/with_history info for purchased signal " <> show event
  liftAff do
    let { tokenId } = unwrap event
    SignalDetails { signal: Signal s } <- getSignalTokenWithHistory (SignalId tokenId)
    s.owner `shouldEqual` account2
    s.radius `shouldEqual` Radius radius

findByHash
  :: forall a.
     HexString
  -> Array (WithMetaData a)
  -> Aff (WithMetaData a)
findByHash hash as =
  let p (WithMetaData {metaData: MetaData {transactionHash}}) =
        transactionHash == hash
  in case head $ filter p as of
       Nothing -> unsafeCrashWith ("Failed to find item with metadata hash " <> show hash)
       Just a -> pure a
