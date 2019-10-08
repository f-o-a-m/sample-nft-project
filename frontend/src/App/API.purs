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
import App.Data.Radius (Radius, radiusFromBigNumber)
import App.Data.SaleId (SaleId, saleIdFromBigNumber)
import App.Data.Signal (Signal(..))
import App.Data.SignalActivity (SignalActivity(..))
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.SignalId (SignalId(..), signalIdFromBigNumber)
import App.Data.Token (zeroToken)
import Control.Error.Util (note)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, decodeJson, getField, (.:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Geohash (Geohash, geohashFromHex, geohashFromLngLat)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..), Minutes(..), fromDuration)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Deploy.Utils (unsafeFromJust)
import Effect.Aff (Aff, delay)
import Effect.Exception (error)
import Foreign.Object (Object)
import Network.Ethereum.Core.BigNumber as BN
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.HexString as H
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Record.Extra (sequenceRecord)

getContracts :: Aff Contracts
getContracts = get "config/contracts" decodeJson

type URLPath = String

get :: forall a. URLPath -> (Json -> Either String a) -> Aff a
get path decode = do
  let
    request = AJ.defaultRequest { url = apiBaseURL <> path, responseFormat = ResponseFormat.json }
    retryPolicy = AJ.defaultRetryPolicy {timeout = Just $ fromDuration $ Minutes 0.4}
  resp <- AJ.retry retryPolicy AJ.request request
  -- resp <- AJ.request request
  case resp.body of
    Left err -> throwAPIError resp "AffJax error" $ printResponseFormatError err
    Right json -> case decode json of
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
signalId1 = unsafeFromJust "SignalId of 1" $ signalIdFromBigNumber $ BN.embed 101

signalId2 :: SignalId
signalId2 = unsafeFromJust "SignalId of 2" $ signalIdFromBigNumber $ BN.embed 102

signalId3 :: SignalId
signalId3 = unsafeFromJust "SignalId of 3" $ signalIdFromBigNumber $ BN.embed 103

saleId2 :: SaleId
saleId2 = unsafeFromJust "SaleId of 2" $ saleIdFromBigNumber $ BN.embed 102
saleId3 :: SaleId
saleId3 = unsafeFromJust "SaleId of 3" $ saleIdFromBigNumber $ BN.embed 103

radius2 :: Radius
radius2 = unsafeFromJust "Radius of 2" $ radiusFromBigNumber $ BN.embed 13
radius3 :: Radius
radius3 = unsafeFromJust "Radius of 3" $ radiusFromBigNumber $ BN.embed 16

signal1 :: Signal
signal1 = Signal
  { id: signalId1
  , stake: zeroToken
  , owner: address1
  , geohash: geohashFromLngLat 10 {lng: 12.0, lat:41.0}
  , radius: radius2
  , sale: Nothing
  }

signal2 :: Signal
signal2 = Signal
  { id: signalId2
  , stake: zeroToken
  , owner: address2
  , geohash: geohashFromLngLat 10 {lng: 12.0, lat:42.0}
  , radius: radius2
  , sale: Nothing
  }

signal3 :: Signal
signal3 = Signal
  { id: signalId3
  , stake: zeroToken
  , owner: address1
  , geohash: geohashFromLngLat 10 {lng: 19.0, lat:22.0}
  , radius: radius3
  , sale: Nothing
  }

getSignals :: Cursor -> Aff (Collection Signal)
getSignals cursor = get
  ("signal_token/with_sales?limit=" <> show (cursor.limit + 1) <> "&offset=" <> show cursor.offset)
  \json -> do
    (jsonArr :: Array (Object Json)) <- decodeJson json
    (items :: Array Signal) <- for jsonArr \itemJson -> do
      signalJson <- itemJson .: "data"
      eSignal <- getField signalJson "eSignal"
      eSaleMb <- getField signalJson "eSale"
      saleInfo <- for eSaleMb \eSale -> do
        owner <- eSale .: "owner"
        sale <- sequenceRecord
          { id: eSale .: "saleID"
          , price: eSale .: "price"
          }
        pure (Tuple owner sale)
      Signal <$> sequenceRecord
        { id: eSignal .: "tokenID"
        , stake: eSignal .: "staked"
        , owner: maybe (eSignal .: "owner") (fst >>> pure) saleInfo
        , geohash: eSignal .: "geohash" >>= decodeGeoHash
        , radius: eSignal .: "radius"
        , sale: pure $ map snd saleInfo
        }
    pure if Array.length items > cursor.limit
      then
        { items: Array.take cursor.limit items
        , next: Just $ cursor{offset = cursor.limit + cursor.offset}
        }
      else
        { items
        , next: Nothing
        }

getSignals' :: Cursor -> Aff (Collection Signal)
getSignals' c = do
  delay $ Milliseconds 1000.0
  pure {items: [signal1, signal2, signal3], next: Just c}

getSignal :: SignalId -> Aff SignalDetails
getSignal (SignalId sid) = get
  ("signal_market/" <> show sid)
  \json -> do
    jsonObj :: Object Json <- decodeJson json
    jsonSignal <- jsonObj .: "signal"
    jsonHistory :: Array (Object Json) <- jsonObj .: "history"
    jsonSignalData <- jsonSignal .: "data"
    activity <- for jsonHistory \itemJson -> do
      jsonData <- itemJson .: "data"
      tag <- jsonData .: "tag"
      contents <- jsonData .: "contents"
      case tag of
        "ListedForSale" ->
          ListedForSale <$> sequenceRecord
            { owner: contents .: "seller"
            , saleId: contents .: "saleID"
            , price: contents .: "price"
            }
        "Sold" -> Sold <$> sequenceRecord
          { owner: contents .: "soldFrom"
          , saleId: contents .: "saleID"
          , price: contents .: "price"
          , buyer: contents .: "soldTo"
          }
        "Unlisted" -> UnlistedFromSale <$> sequenceRecord
          { owner: contents .: "owner"
          , saleId: contents .: "saleID"
          }
        _ -> throwError $ "Invalid tag: " <> show tag
    let
      saleInfo = Array.head activity >>= case _ of
        ListedForSale { owner, saleId, price } -> Just $ Tuple owner { id: saleId, price }
        Sold _ -> Nothing
        UnlistedFromSale _ -> Nothing
    signal <- Signal <$> sequenceRecord
      { id: jsonSignalData .: "tokenID"
      , stake: jsonSignalData .: "staked"
      , owner: maybe (jsonSignalData .: "owner") (fst >>> pure) saleInfo
      , geohash: jsonSignalData .: "geohash" >>= decodeGeoHash
      , radius: jsonSignalData .: "radius"
      , sale: pure $ map snd saleInfo
      }
    pure $ SignalDetails {signal, activity}


getSignal' :: SignalId -> Aff SignalDetails
getSignal' sid = do
  delay $ Milliseconds 1000.0
  pure $ SignalDetails
    { signal: signal1
    , activity:
        [ ListedForSale
            { owner: address2
            , saleId: saleId2
            , price: zeroToken
            }
        , Sold
            { owner: address2
            , saleId: saleId3
            , price: zeroToken
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
            }
        , SignalListedForSale
            { owner: address2
            , saleId: saleId2
            , signal: signal1
            , price: zeroToken
            }
        , SignalSold
            { owner: address2
            , saleId: saleId3
            , buyer: address1
            , signal: signal1
            , price: zeroToken
            }
        ]
    , next: Just c
    }


decodeGeoHash :: String -> Either String Geohash
decodeGeoHash str = note "Invalid GeoHash" (H.mkHexString str) <#> geohashFromHex
