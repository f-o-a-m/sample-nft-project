{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.Models.FoamTokenTransfer where

import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Field, Select, selectTable, runSelect, runInsertMany, constant,
                         Table, table, tableField, SqlText, SqlNumeric, ToFields)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           Data.Int (Int64)
import           SignalMarket.Common.EventTypes (EventId, Value, EthAddress)

-- FoamToken Transfer

data Transfer' to from value = Transfer
  { to :: to
  , from :: from
  , value :: value
  }

$(makeAdaptorAndInstance "pTransfer" ''Transfer')

type TransferPG = Transfer' (Field SqlText) (Field SqlText) (Field SqlNumeric)
type Transfer = Transfer' EthAddress EthAddress Value

transferTable :: Table TransferPG TransferPG
transferTable = table "transfer"
                       (pTransfer Transfer { to = tableField "to"
                                           , from  = tableField "from" 
                                           , value = tableField "value"
                                           }
                       )

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
inserter conn table a = runInsertMany conn table [constant a]
