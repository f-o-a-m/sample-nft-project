{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.Models.SignalTokenTrackedToken where

import           Opaleye (Field, Table, table, tableField, SqlText, SqlNumeric, SqlBytea)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           SignalMarket.Common.EventTypes (EthAddress, ByteNValue, Value, TokenID)
-- SignalToken
-- TrackedToken :: {cst :: (BytesN (D3 :& DOne D2)),nftAddress :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6)),geohash :: (BytesN (D3 :& DOne D2)),radius :: (UIntN (D2 :& D5 :& DOne D6))}

data TrackedToken' nftAddress cst geohash radius tokenID = TrackedToken
  { nftAddress :: nftAddress
  , cst :: cst
  , geohash :: geohash
  , radius :: radius
  , tokenID :: tokenID
  }

$(makeAdaptorAndInstance "pTrackedToken" ''TrackedToken')

type TrackedTokenPG = TrackedToken' (Field SqlText) (Field SqlBytea) (Field SqlBytea) (Field SqlNumeric) (Field SqlNumeric)
type TrackedToken = TrackedToken' EthAddress ByteNValue ByteNValue Value TokenID

trackedTokenTable :: Table TrackedTokenPG TrackedTokenPG
trackedTokenTable = table "trackedToken"
                           (pTrackedToken TrackedToken { nftAddress = tableField "nftAddress"
                                                       , cst = tableField "cst"
                                                       , geohash = tableField "geohash"
                                                       , radius = tableField "radius"
                                                       , tokenID = tableField "tokenID"
                                                       }
                           )
