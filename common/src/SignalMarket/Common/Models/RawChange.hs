{-# LANGUAGE TemplateHaskell #-}

module SignalMarket.Common.Models.RawChange where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EthAddress, EventID,
                                                 HexInteger, HexString)

data RawChange' li txh bh addr eid = RawChange
  { logIndex        :: li
  , transactionHash :: txh
  , blockHash       :: bh
  , address         :: addr
  , eventID         :: eid
  } deriving Generic


$(makeAdaptorAndInstance "pRawChange" ''RawChange')

type RawChangePG = RawChange' (Field SqlNumeric) (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText)
type RawChange = RawChange' HexInteger HexString HexString EthAddress EventID

rawChangeTable :: Table RawChangePG RawChangePG
rawChangeTable = table "raw_change"
                       (pRawChange RawChange { logIndex = tableField "log_index"
                                             , transactionHash  = tableField "transaction_hash"
                                             , blockHash = tableField "block_hash"
                                             , address = tableField "address"
                                             , eventID = tableField "event_id"
                                             }
                       )

instance A.ToJSON RawChange where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON RawChange where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject RawChange

instance K.LogItem RawChange where
  payloadKeys _ _ = K.AllKeys
