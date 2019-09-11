{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.SignalTokenModel where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
-- import Data.Solidity.Prim.Address (Address)
import           Opaleye (Field, Select, selectTable, runSelect, runInsertMany, constant,
                         Table, table, tableField, SqlText, SqlNumeric, SqlBytea, ToFields)
import           Data.ByteString (ByteString)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           Data.Scientific (Scientific)
import           Opaleye.RunQuery (QueryRunnerColumnDefault)
import           Data.Int (Int64)

newtype EventId = EventId Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

newtype Value = Value Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)
-- @NOTE: maybe this can be the same thing; `Transfer` and `TrackedToken` name this field differently
newtype TokenID = TokenID Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)

newtype EthAddress = EthAddress Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)


--------------------------------------------------------------------------------
{-
SignalToken
- Transfer :: {_from :: Address,_to :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}
- TrackedToken :: {cst :: (BytesN (D3 :& DOne D2)),nftAddress :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6)),geohash :: (BytesN (D3 :& DOne D2)),radius :: (UIntN (D2 :& D5 :& DOne D6))}

-}

data Transfer' to from tokenId = Transfer
  { to :: to
  , from :: from
  , tokenId :: tokenId
  }

$(makeAdaptorAndInstance "pTransfer" ''Transfer')

type TransferPG = Transfer' (Field SqlText) (Field SqlText) (Field SqlNumeric)
type Transfer = Transfer' Text Text Scientific

transferTable :: Table TransferPG TransferPG
transferTable = table "transfer"
                       (pTransfer Transfer { to = tableField "to"
                                           , from  = tableField "from"
                                           , tokenId = tableField "tokenId"
                                           }
                       )

data TrackedToken' nftAddress cst geohash radius tokenID = TrackedToken
  { nftAddress :: nftAddress
  , cst :: cst
  , geohash :: geohash
  , radius :: radius
  , tokenID :: tokenID
  }

$(makeAdaptorAndInstance "pTrackedToken" ''TrackedToken')

type TrackedTokenPG = TrackedToken' (Field SqlText) (Field SqlBytea) (Field SqlBytea) (Field SqlNumeric) (Field SqlNumeric)
type TrackedToken = TrackedToken' Text ByteString ByteString Scientific Scientific

trackedTokenTable :: Table TrackedTokenPG TrackedTokenPG
trackedTokenTable = table "trackedToken"
                           (pTrackedToken TrackedToken { nftAddress = tableField "nftAddress"
                                                        , cst = tableField "cst"
                                                        , geohash = tableField "geohash"
                                                        , radius = tableField "radius"
                                                        , tokenID = tableField "tokenID"
                                                        }
                           )

--------------------------------------------------------------------------------

transferSelect :: Select TransferPG
transferSelect = selectTable transferTable

runTransferSelect :: Connection -> IO [Transfer]
runTransferSelect conn = runSelect conn transferSelect

insertTransfer :: Connection -> Transfer -> IO Int64
insertTransfer conn transfer = runInsertMany conn transferTable [constant transfer]

inserter
  :: Default ToFields haskells fields
  => Connection
  -> Table fields fields
  -> haskells
  -> IO Int64
inserter conn table a =
 runInsertMany conn table [constant a]
