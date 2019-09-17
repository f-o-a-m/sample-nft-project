{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.FoamTokenTransfer where

import qualified Data.Aeson                      as A
import           Data.Int                        (Int64)
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Database.PostgreSQL.Simple      (Connection)
import           GHC.Generics                    (Generic)
import qualified Katip                           as K
import           Opaleye                         (Field, Select, SqlNumeric,
                                                  SqlText, Table, ToFields,
                                                  constant, runInsertMany,
                                                  runSelect, selectTable, table,
                                                  tableField)
import           SignalMarket.Common.Aeson       (defaultAesonOptions)
import           SignalMarket.Common.EventTypes  (EthAddress, EventID, Value)

-- FoamToken Transfer

data Transfer' to from value eventID = Transfer
  { to      :: to
  , from    :: from
  , value   :: value
  , eventID :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pTransfer" ''Transfer')

type TransferPG = Transfer' (Field SqlText) (Field SqlText) (Field SqlNumeric) (Field SqlText)
type Transfer = Transfer' EthAddress EthAddress Value EventID

transferTable :: Table TransferPG TransferPG
transferTable = table "foam_token_transfer"
                       (pTransfer Transfer { to = tableField "to"
                                           , from  = tableField "from"
                                           , value = tableField "value"
                                           , eventID = tableField "event_id"
                                           }
                       )

instance A.ToJSON Transfer where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON Transfer where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject Transfer

instance K.LogItem Transfer where
  payloadKeys _ _ = K.AllKeys

insertTransfer :: Connection -> Transfer -> IO Int64
insertTransfer conn transfer = runInsertMany conn transferTable [constant transfer]

