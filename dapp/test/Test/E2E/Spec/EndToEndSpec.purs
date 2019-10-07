module Test.E2E.Spec.End2EndSpec where

import Prelude

import Chanterelle.Test (assertWeb3)
import Contracts.FoamToken as FoamToken
import Control.Coroutine as C
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Data.Array ((!!))
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
import Partial.Unsafe (unsafePartial)
import Test.Actions (faucetTokens)
import Test.E2E.End2EndConfig (End2EndConfig)
import Test.Spec (SpecT, beforeAll_, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (TestEnv, go, mkSignalAttrGen)
import Test.Websocket (mkMonitor)
import Type.Proxy (Proxy(..))


--transferFOAM
--  :: forall r.
--     { clientEnv :: ClientEnv
--     , ws :: WebSocket
--     | r
--     }
--  -> { from :: Address
--     , to :: Address
--     , value :: Int
--     }
--  -> Aff Unit
--transferFOAM {clientEnv, ws} {to, from, value} = do

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
        sendTokens cfg {to: account1, from: account2, amount: 1}
      it "can track signal token creation" do
        pure unit
      it "can track signals for sale" do
        pure unit
      it "can track signals sold" do
        pure unit

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
  -> { to :: Address
     , from :: Address
     , amount :: Int
     }
  -> m Unit
sendTokens cfg@{clientEnv, contractAddresses} {to, from, amount} = do
  let {foamToken} = contractAddresses
      txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ from
      action = FoamToken.transfer txOpts { amount: mkUIntN s256 amount, recipient: to }
  {change, event} <- monitorUntilEvent cfg (Proxy :: Proxy FoamToken.Transfer) {action, emittingAddress: foamToken}
  let FoamToken.Transfer {from: _from, to: _to} = event
  liftAff do
    from `shouldEqual` _from
    to `shouldEqual` _to

