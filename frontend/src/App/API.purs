module App.API
  ( getContracts
  , getSignals
  , getSignal
  , getActivity
  ) where

import Prelude

import Affjax (printResponseFormatError)
import Affjax as AJ
import Affjax.ResponseFormat as ResponseFormat
import App.Data.Activity (Activity(..))
import App.Data.Collections (Cursor, Collection)
import App.Data.Contracts (Contracts)
import App.Data.Radius (Radius(..))
import App.Data.SaleId (SaleId, saleIdFromBigNumber)
import App.Data.Signal (Signal(..))
import App.Data.SignalActivity (SignalActivity(..))
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.SignalId (SignalId, signalIdFromBigNumber)
import App.Data.Token (zeroToken)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.DateTime (adjust)
import Data.Either (Either(..))
import Data.Geohash (geohashFromLngLat)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..), Milliseconds(..), Minutes(..), fromDuration)
import Deploy.Utils (unsafeFromJust)
import Effect.Aff (Aff, delay)
import Effect.Exception (error)
import Effect.Now (nowDateTime)
import Effect.Unsafe (unsafePerformEffect)
import Network.Ethereum.Core.BigNumber as BN
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (Address, mkAddress)

getContracts :: Aff Contracts
getContracts = get "config/contracts"

type URLPath = String

get :: forall a. DecodeJson a => URLPath -> Aff a
get path = do
  let
    request = AJ.defaultRequest { url = apiBaseURL <> path, responseFormat = ResponseFormat.json }
    retryPolicy = AJ.defaultRetryPolicy {timeout = Just $ fromDuration $ Minutes 0.4}
  resp <- AJ.retry retryPolicy AJ.request request
  -- resp <- AJ.request request
  case resp.body of
    Left err -> throwAPIError resp "AffJax error" $ printResponseFormatError err
    Right json -> case decodeJson json of
      Left err -> throwAPIError resp "DecodeJson error" err
      Right res -> pure res
  where
    throwAPIError resp msg err = throwError $ error $ msg <> ": " <> show
      { error: err
      , status: resp.status
      , statusText: resp.statusText
      , headers: resp.headers
      }

foreign import apiBaseURL :: String

address1 :: Address
address1 = unsafeFromJust "Must be valid Address 000...1"
  $ mkAddress =<< mkHexString "0x5e8A927093a4465f2fFFb466f0594BfB2E04C204"
address2 :: Address
address2 = unsafeFromJust "Must be valid Address 000...2"
  $ mkAddress =<< mkHexString "0x0000000000000000000000000000000000000002"
address3 :: Address
address3 = unsafeFromJust "Must be valid Address 000...3"
  $ mkAddress =<< mkHexString "0x0000000000000000000000000000000000000003"
address4 :: Address
address4 = unsafeFromJust "Must be valid Address 000...4"
  $ mkAddress =<< mkHexString "0x0000000000000000000000000000000000000004"
address5 :: Address
address5 = unsafeFromJust "Must be valid Address 000...5"
  $ mkAddress =<< mkHexString "0x0000000000000000000000000000000000000005"
address6 :: Address
address6 = unsafeFromJust "Must be valid Address 000...6"
  $ mkAddress =<< mkHexString "0x0000000000000000000000000000000000000006"

signalId1 :: SignalId
signalId1 = unsafeFromJust "SignalId of 1" $ signalIdFromBigNumber $ BN.embed 1

signalId2 :: SignalId
signalId2 = unsafeFromJust "SignalId of 2" $ signalIdFromBigNumber $ BN.embed 2

saleId2 :: SaleId
saleId2 = unsafeFromJust "SaleId of 2" $ saleIdFromBigNumber $ BN.embed 2
saleId3 :: SaleId
saleId3 = unsafeFromJust "SaleId of 3" $ saleIdFromBigNumber $ BN.embed 3

signal1 :: Signal
signal1 = Signal
  { id: signalId1
  , stake: zeroToken
  , owner: address1
  , geohash: geohashFromLngLat 10 {lng: 12.0, lat:41.0}
  , radius: Radius 11.0
  , sale: Nothing
  }

signal2 :: Signal
signal2 = Signal
  { id: signalId2
  , stake: zeroToken
  , owner: address2
  , geohash: geohashFromLngLat 10 {lng: 12.0, lat:42.0}
  , radius: Radius 12.0
  , sale: Nothing
  }

getSignals :: Cursor -> Aff (Collection Signal)
getSignals c = do
  delay $ Milliseconds 1000.0
  pure {items: [signal1, signal2], next: Just c}

getSignal :: SignalId -> Aff SignalDetails
getSignal sid = do
  delay $ Milliseconds 1000.0
  pure $ SignalDetails
    { signal: signal1
    , activity:
        [ ListedForSale
            { owner: address2
            , saleID: saleId2
            , price: zeroToken
            , timestamp: unsafeFromJust "adjust is good" $ adjust (Days (-3.0)) (unsafePerformEffect nowDateTime)
            }
        , Soled
            { owner: address2
            , saleID: saleId3
            , price: zeroToken
            , timestamp: unsafeFromJust "adjust is good" $ adjust (Days (-2.0)) (unsafePerformEffect nowDateTime)
            , buyer: address1
            }
        ]
    }

getActivity :: Cursor -> Aff (Collection Activity)
getActivity c = do
  delay $ Milliseconds 1000.0
  pure
    { items:
        [ TokenTransfer
            { from: address3
            , to: address3
            , amount: zeroToken
            , timestamp: unsafeFromJust "adjust is good" $ adjust (Days (-6.0)) (unsafePerformEffect nowDateTime)
            }
        , SignalListedForSale
            { owner: address2
            , saleID: saleId2
            , timestamp: unsafeFromJust "adjust is good" $ adjust (Days (-3.0)) (unsafePerformEffect nowDateTime)
            , signal: signal1
            , price: zeroToken
            }
        , SignalSoled
            { owner: address2
            , saleID: saleId3
            , timestamp: unsafeFromJust "adjust is good" $ adjust (Days (-2.0)) (unsafePerformEffect nowDateTime)
            , buyer: address1
            , signal: signal1
            , price: zeroToken
            }
        ]
    , next: Nothing
    }
