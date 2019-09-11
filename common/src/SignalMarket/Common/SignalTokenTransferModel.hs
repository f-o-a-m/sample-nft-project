{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.SignalTokenTransferModel where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
-- import Data.Solidity.Prim.Address (Address)
import           Opaleye (Field, Select, selectTable, runSelect, runInsertMany, constant,
                         Table, table, tableField, SqlText, SqlNumeric, SqlBytea, ToFields)
import           Data.ByteString (ByteString)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           Data.Scientific (Scientific)
import           Data.Int (Int64)

-- SignalToken
-- Transfer :: {_from :: Address,_to :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

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

transferSelect :: Select TransferPG
transferSelect = selectTable transferTable

runTransferSelect :: Connection -> IO [Transfer]
runTransferSelect conn = runSelect conn transferSelect

insertTransfer :: Connection -> Transfer -> IO Int64
insertTransfer conn transfer = runInsertMany conn transferTable [constant transfer]
