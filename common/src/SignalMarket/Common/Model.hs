{-# LANGUAGE TemplateHaskell #-}

module SignalMarket.Common.Model where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
-- import Data.Solidity.Prim.Address (Address)
import           Opaleye (Field, Select, selectTable, runSelect, runInsertMany,
                         Table, table, tableField, SqlText, SqlNumeric)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import            Data.Scientific (Scientific)
import Opaleye.RunQuery (QueryRunnerColumnDefault)
import Data.Int (Int64)

newtype EventId = EventId Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

newtype Value = Value Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)

newtype EthAddress = EthAddress Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)


--------------------------------------------------------------------------------
{-
FoamToken
- Transfer

SignalToken
- Transfer
- TrackedToken

SignalMarket
- ForSale
- Bought
- Unlisted


-}
data Transfer' to from value = Transfer
  { to :: to
  , from :: from
  , value :: value
  }

$(makeAdaptorAndInstance "pTransfer" ''Transfer')

type TransferPG = Transfer' (Field SqlText) (Field SqlText) (Field SqlNumeric)
type Transfer = Transfer' Text Text Scientific

transferTable :: Table TransferPG TransferPG
transferTable = table "transfer"
                       (pTransfer Transfer { to = tableField "to"
                                           , from  = tableField "from" 
                                           , value = tableField "value"
                                           }
                       )

--------------------------------------------------------------------------------

transferSelect :: Select TransferPG
transferSelect = selectTable transferTable

runTransferSelect :: Connection -> IO [Transfer]
runTransferSelect conn = runSelect conn transferSelect

insertTransfer :: Connection -> Transfer -> IO Int64
insertTransfer conn transfer = runInsertMany conn transferTable [transfer]