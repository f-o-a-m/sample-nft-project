module App.API
  ( getContracts
  , getSignalTokenWithSales
  , getSignalTokenWithHistory
  , foamTokenTransfers
  , signalTokenWithSales
  , AddressP(..)
  ) where

import Prelude

import App.API.Internal (apiBaseURL)
import App.Data.Collections (Cursor, Collection)
import App.Data.Radius (Radius, radiusFromBigNumber)
import App.Data.SaleId (SaleId, saleIdFromBigNumber)
import App.Data.Signal (Signal(..))
import App.Data.SignalActivity as Activity
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.SignalId (SignalId, signalIdFromBigNumber)
import App.Data.Token (Token, tokenFromBigNumber, zeroToken)
import App.MarketClient.Client (BlockNumberOrdering, ClientM, Contracts, FoamTokenTransfer, WithMetaData(..), runClientM)
import App.MarketClient.Types (ESale(..), ESignal(..), SignalMarketHistoryEntry(..), SignalTokenWithHistory(..), SignalTokenWithSale(..))
import Control.Error.Util (note)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Geohash (geohashFromHex, geohashFromLngLat)
import Data.Maybe (Maybe(..), maybe)
import Data.String (codePointFromChar, takeWhile)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Deploy.Utils (unsafeFromJust)
import Effect.Aff (Aff, delay)
import Effect.Exception (error)
import Network.Ethereum.Core.BigNumber as BN
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Network.Ethereum.Web3.Types (ETHER)
import Servant.API (type (:>))
import Servant.API as API
import Servant.Client (ClientEnv(..))
import Servant.Client as Client

type URLPath = String

-- contract/config
type GetContracts =
     API.S "config"
  :> API.S "contracts"
  :> API.GET Json Contracts

contracts :: ClientM Contracts
contracts =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetContracts)

getContracts :: Aff Contracts
getContracts = getM contracts pure

-- foam token transfers
newtype AddressP = AddressP Address

instance encodeAddressPQueryParamData :: API.EncodeQueryParam AddressP where
  encodeQueryParam (AddressP a) = show a

type GetFoamTokenTransfers =
     API.S "foam_token"
  :> API.S "transfers"
  :> API.QPs ( to :: Array AddressP
             , from :: Array AddressP
             , limit :: Maybe Int
             , offset :: Maybe Int
             , ordering :: Maybe BlockNumberOrdering
             )
  :> API.GET Json (Array (WithMetaData FoamTokenTransfer))

foamTokenTransfers
  :: API.QueryParams ( to :: Array AddressP
                     , from :: Array AddressP
                     , limit :: Maybe Int
                     , offset :: Maybe Int
                     , ordering :: Maybe BlockNumberOrdering
                     )
  -> ClientM (Array (WithMetaData FoamTokenTransfer))
foamTokenTransfers =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetFoamTokenTransfers)

-- signal tokens
---- with sales
type GetSignalTokenWithSales =
     API.S "signal_token"
  :> API.S "with_sales"
  :> API.QPs ( limit :: Maybe Int
             , offset :: Maybe Int
             )
  :> API.GET Json (Array (WithMetaData SignalTokenWithSale))

signalTokenWithSales
  :: API.QueryParams ( limit :: Maybe Int
                     , offset :: Maybe Int
                     )
  -> ClientM (Array (WithMetaData SignalTokenWithSale))
signalTokenWithSales =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetSignalTokenWithSales)

getSignalTokenWithSales :: Cursor -> Aff (Collection Signal)
getSignalTokenWithSales cursor =
  getM (signalTokenWithSales params) (toResponse cursor)
  where
    params = API.QueryParams { limit: Just cursor.limit
                             , offset: Just cursor.offset
                             }

    eSaleMbToSaleInfo
      :: Maybe ESale
      -> Maybe (Tuple Address
                { id :: SaleId
                , price :: Token ETHER
                })
    eSaleMbToSaleInfo eSaleMb =
      case eSaleMb of
        Nothing -> Nothing
        Just (ESale eSale@{owner}) -> do
          id <- saleIdFromBigNumber eSale.saleID
          price <- tokenFromBigNumber eSale.price
          pure $ Tuple owner { id, price }

    signalFromSale
      :: WithMetaData SignalTokenWithSale
      -> Either String Signal
    signalFromSale (WithMetaData { "data": SignalTokenWithSale { eSale, eSignal }}) = do
      let ESignal eSignal@{owner} = eSignal
          geohash = geohashFromHex eSignal.geohash
          saleInfo = eSaleMbToSaleInfo eSale
          sale = map snd saleInfo
      id <- note "Invalid TokenID" (signalIdFromBigNumber eSignal.tokenID)
      radius <- note "Invalid Radius" (radiusFromBigNumber eSignal.radius)
      stake <- note "Invalid Stake" (tokenFromBigNumber eSignal.staked)
      owner <- maybe (pure eSignal.owner) (fst >>> pure) saleInfo
      pure $ Signal { id, stake, radius, owner, geohash, sale }

    toResponse
      :: Cursor
      -> Array (WithMetaData SignalTokenWithSale)
      -> Either String (Collection Signal)
    toResponse c@{ limit, offset } arr = do
      items <- for arr \s -> signalFromSale s
      pure if Array.length items > limit
           then { items: Array.take limit items
                , next: Just $ c{offset = limit + offset}
                }
           else { items, next: Nothing }

-- signal market
type GetSignalMarketSignalHistory =
     API.S "signal_market"
  :> API.CAP "token_id" SignalId
  :> API.GET Json SignalTokenWithHistory

signalMarketSignalHistory :: API.Capture "token_id" SignalId -> ClientM SignalTokenWithHistory
signalMarketSignalHistory =
  Client.makeClientRoute (API.RouteProxy :: API.RouteProxy GetSignalMarketSignalHistory)

getSignalTokenWithHistory :: SignalId -> Aff SignalDetails
getSignalTokenWithHistory signalId =
  getM (signalMarketSignalHistory captureArg) toResponse
  where
    captureArg = API.capture (SProxy :: SProxy "token_id") signalId

    historyToActivity
      :: WithMetaData SignalMarketHistoryEntry
      -> Either String Activity.SignalActivity
    historyToActivity (WithMetaData { "data": entry }) = do
      case entry of
        ListedForSale listed -> do
          saleId <- note "Invalid saleID" (saleIdFromBigNumber listed.saleID)
          price <- note "Invalid price" (tokenFromBigNumber listed.price)
          pure $ Activity.ListedForSale { owner: listed.seller, saleId, price }
        Sold sold -> do
          saleId <- note "Invalid saleID" (saleIdFromBigNumber sold.saleID)
          price <- note "Invalid price" (tokenFromBigNumber sold.price)
          pure $ Activity.Sold { owner: sold.soldFrom, buyer: sold.soldTo, saleId, price }
        Unlisted {saleID, owner} -> do
          saleId <- note "Invalid saleID" (saleIdFromBigNumber saleID)
          pure $ Activity.UnlistedFromSale { owner, saleId }

    eSignalToSignal
      :: WithMetaData ESignal
      -> Maybe (Tuple Address { id :: SaleId, price :: Token ETHER })
      -> Either String Signal
    eSignalToSignal (WithMetaData { "data": ESignal eSignal }) saleInfo = do
      let geohash = geohashFromHex eSignal.geohash
          sale = map snd saleInfo
      id <- note "Invalid TokenID" (signalIdFromBigNumber eSignal.tokenID)
      radius <- note "Invalid Radius" (radiusFromBigNumber eSignal.radius)
      stake <- note "Invalid Stake" (tokenFromBigNumber eSignal.staked)
      owner <- maybe (pure eSignal.owner) (fst >>> pure) saleInfo
      pure $ Signal { id, stake, owner, geohash, radius, sale }

    toResponse :: SignalTokenWithHistory -> Either String SignalDetails
    toResponse (SignalTokenWithHistory stwh) = do
      activity <- for stwh.history historyToActivity
      let
        saleInfo = Array.head activity >>= case _ of
          Activity.ListedForSale { owner, saleId, price } ->
            Just $ Tuple owner { id: saleId, price }
          Activity.Sold _ -> Nothing
          Activity.UnlistedFromSale _ -> Nothing
      signal <- eSignalToSignal stwh.signal saleInfo
      pure $ SignalDetails { signal, activity }

-------

getM :: forall a r. ClientM r -> (r -> Either String a) -> Aff a
getM clientRoute f = do
  let clientEnv =
        -- protocol is suffixed with a `:`
        -- the env already concats with a `:`
        ClientEnv { protocol:
                    takeWhile (_ /= codePointFromChar ':') apiBaseURL.protocol
                  , baseURL: apiBaseURL.baseURL
                  }
  resp <- runClientM clientEnv clientRoute
  case resp of
    Left err -> throwError (error $ Client.errorToString err)
    Right val -> case f val of
      Left err -> throwError (error err)
      Right res -> pure res

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

getSignal' :: SignalId -> Aff SignalDetails
getSignal' sid = do
  delay $ Milliseconds 1000.0
  pure $ SignalDetails
    { signal: signal1
    , activity:
        [ Activity.ListedForSale
            { owner: address2
            , saleId: saleId2
            , price: zeroToken
            }
        , Activity.Sold
            { owner: address2
            , saleId: saleId3
            , price: zeroToken
            , buyer: address1
            }
        ]
    }
