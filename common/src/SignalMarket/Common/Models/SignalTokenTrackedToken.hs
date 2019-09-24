{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalTokenTrackedToken where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (ByteNValue, EthAddress,
                                                 EventID, TokenID, Value)
-- SignalToken
-- TrackedToken :: {cst :: (BytesN (D3 :& DOne D2)),nftAddress :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6)),geohash :: (BytesN (D3 :& DOne D2)),radius :: (UIntN (D2 :& D5 :& DOne D6))}

data TrackedToken' nftAddress cst geohash radius tokenID eventID = TrackedToken
  { nftAddress :: nftAddress
  , cst        :: cst
  , geohash    :: geohash
  , radius     :: radius
  , tokenID    :: tokenID
  , eventID    :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pTrackedToken" ''TrackedToken')

type TrackedTokenPG = TrackedToken' (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlNumeric) (Field SqlNumeric) (Field SqlText)
type TrackedToken = TrackedToken' EthAddress ByteNValue ByteNValue Value TokenID EventID

trackedTokenTable :: Table TrackedTokenPG TrackedTokenPG
trackedTokenTable = table "signal_token_tracked_token"
                           (pTrackedToken TrackedToken { nftAddress = tableField "nft_address"
                                                       , cst = tableField "cst"
                                                       , geohash = tableField "geohash"
                                                       , radius = tableField "radius"
                                                       , tokenID = tableField "token_id"
                                                       , eventID = tableField "event_id"
                                                       }
                           )

instance A.ToJSON TrackedToken where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON TrackedToken where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject TrackedToken

instance K.LogItem TrackedToken where
  payloadKeys _ _ = K.AllKeys
