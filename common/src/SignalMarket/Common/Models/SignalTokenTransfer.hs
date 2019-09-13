{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.Models.SignalTokenTransfer where

import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Field, Table, table, tableField, SqlText, SqlNumeric, ToFields)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           Data.Int (Int64)
import           SignalMarket.Common.EventTypes (EthAddress, TokenID)

-- SignalToken
-- Transfer :: {_from :: Address,_to :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

data Transfer' to from tokenID = Transfer
  { to :: to
  , from :: from
  , tokenID :: tokenID
  }

$(makeAdaptorAndInstance "pTransfer" ''Transfer')

type TransferPG = Transfer' (Field SqlText) (Field SqlText) (Field SqlNumeric)
type Transfer = Transfer' EthAddress EthAddress TokenID

transferTable :: Table TransferPG TransferPG
transferTable = table "transfer"
                       (pTransfer Transfer { to = tableField "to"
                                           , from  = tableField "from"
                                           , tokenID = tableField "tokenID"
                                           }
                       )
