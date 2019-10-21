module E2E.Spec.End2EndSpec where

import Prelude

import App.GraphQLApi (getSignalOwnerStats)
import Chanterelle.Test (assertWeb3)
import Contracts.FoamToken as FoamToken
import Control.Coroutine as C
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array (filter, head, (!!))
import Data.Foldable (length)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Deploy.Utils (mkUIntN)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Network.Ethereum.Web3 (class EventFilter, Address, Change(..), HexString, Web3, _from, _to, defaultTransactionOptions, eventFilter)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Servant.API as API
import E2E.End2EndConfig (End2EndConfig)
import Test.Actions (faucetTokens)
import Test.MarketClient.Client (MetaData(..))
import Test.MarketClient.Client as Client
import Test.Spec (SpecT, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (TestEnv, go, mkSignalAttrGen)
import App.Websocket (mkMonitor)
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
          { to: [Client.AddressP to]
          , from: [Client.AddressP from]
          , limit: Just 100
          , offset: Just 0
          , ordering: Just Client.DESC
          }
    results <- Client.assertClientM clientEnv $ Client.foamTokenTransfers params
    Client.WithMetaData {"data": Client.FoamTokenTransfer transfer} <- findByHash transactionHash results
    from `shouldEqual` transfer.from
    to `shouldEqual` transfer.to

findByHash
  :: forall a.
     HexString
  -> Array (Client.WithMetaData a)
  -> Aff (Client.WithMetaData a)
findByHash hash as =
  let p (Client.WithMetaData {metaData: MetaData {transactionHash}}) =
        transactionHash == hash
  in case head $ filter p as of
       Nothing -> unsafeCrashWith ("Failed to find item with metadata hash " <> show hash)
       Just a -> pure a

