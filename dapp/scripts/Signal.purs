
module Signal
( main
) where

import Prelude

import App.API (getContracts)
import App.Data.Contracts (Contracts(..))
import Chanterelle.Internal.Utils (getPrimaryAccount)
import Chanterelle.Test (assertWeb3)
import Contracts.FoamToken as FoamToken
import Contracts.SignalToken as SignalToken
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Parallel (parTraverse_)
import Data.Array (range)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Geohash (geohashFromLngLat, geohashToBS32)
import Data.Int (round)
import Data.Int as Int
import Data.Maybe (fromMaybe, maybe)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Error, error, launchAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Random (randomRange)
import Etherium.Tx (awaitMined)
import Etherium.TxOpts (txOpts)
import Network.Ethereum.Web3 (Value, eventFilter, httpProvider, mkAddress, mkHexString, mkValue, toMinorUnit, uIntNFromBigNumber)
import Network.Ethereum.Web3 as BN
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types.EtherUnit (Ether)
import Node.Process (lookupEnv)
import Test.Utils (monitorUntil)
import Type.Proxy (Proxy(..))
import Unsafe (unsafeFromJust)

main :: Effect Unit
main = void <<< launchAff $ do
  Contracts {signalToken, foamToken} <- getContracts
  owner <- liftEffect $ lookupEnv "OWNER_ADDRESS" >>= \addr -> maybe
      (throw "Missing Environment Variable 'OWNER_ADDRESS'")
      pure
      (addr >>= mkHexString >>= mkAddress)
  signalCount <- liftEffect $ lookupEnv "SIGNAL_COUNT" <#> \n -> fromMaybe 1 $ n >>= Int.fromString
  provider <- liftEffect $ httpProvider "http://localhost:8545"
  from <- assertWeb3 provider getPrimaryAccount
  signals <- for (range 1 signalCount) \n -> liftEffect do
    lng <- randomRange 2.0 20.0
    lat <- randomRange 2.0 20.0
    radius <- map round $ randomRange 1.0 25.0
    stake <- map round $ randomRange 1.0 4.0
    let geohash = geohashToBS32 $ geohashFromLngLat 5 {lng, lat}
    pure {geohash, radius, stake }
  assertWeb3 provider do
    let
      totalStake' = sum $ map (_.stake) signals
      totalStake = unsafeFromJust "invalid stake" $ uIntNFromBigNumber s256 $ toMinorUnit (mkValue (BN.embed totalStake') :: Value Ether)
    log "approve submitting"
    txHash <- FoamToken.approve (txOpts {from, to: foamToken}) { spender: signalToken, value: totalStake}
    log "approve mining"
    expectRight =<< liftAff (awaitMined txHash provider)
    signals # parTraverse_ \signal -> do
      log $ "mintSignal " <> show signal
      SignalToken.TrackedToken {tokenID} <- monitorUntil provider (SignalToken.mintSignal
        (txOpts {from, to: signalToken})
        { owner: from
        , geohash: signal.geohash
        , radius: unsafeFromJust "invalid radius" $ uIntNFromBigNumber s256 $ BN.embed signal.radius
        , stake: unsafeFromJust "invalid stake" $ uIntNFromBigNumber s256 $ toMinorUnit (mkValue (BN.embed signal.stake) :: Value Ether)
        })
        (eventFilter (Proxy :: Proxy SignalToken.TrackedToken) signalToken)
      log $ "transfer submitting " <> show signal
      txHash' <- SignalToken.transferFrom
        (txOpts {from, to: signalToken})
        { from
        , to: owner
        , tokenId: tokenID
        }
      expectRight =<< liftAff (awaitMined txHash' provider)
      log $ "transfer mining " <> show signal


expectRight
  :: forall a m b.
     MonadError Error m
  => Show a
  => Either a b
  -> m b
expectRight = case _ of
  Left l -> throwError $ error $ "Expected Right but got (Left " <> show l <>")"
  Right r -> pure r
