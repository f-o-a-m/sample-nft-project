{-# LANGUAGE TemplateHaskell #-}
module SignalMarket.Common.Models.RawChange where

import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           Data.Text                      (Text)
import           Database.PostgreSQL.Simple     (Connection)
import           Opaleye                        (Field, Select, SqlNumeric,
                                                 SqlText, Table, ToFields,
                                                 constant, runInsertMany,
                                                 runSelect, selectTable, table,
                                                 tableField)
import           SignalMarket.Common.EventTypes (EthAddress, EventID,
                                                 HexInteger)

{-

data Change = Change
  { changeLogIndex         :: HexInteger
  , changeTransactionHash  :: HexInteger
  , changeBlockHash        :: Text
  , changeAddress          :: EthAddress
  } deriving (Eq, Show, Generic)

-}

data RawChange' li txh bh addr eid = RawChange
  { logIndex        :: li
  , transactionHash :: txh
  , blockHash       :: bh
  , address         :: addr
  , eventID         :: eid
  }


$(makeAdaptorAndInstance "pRawChange" ''RawChange')

type RawChangePG = RawChange' (Field SqlNumeric) (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText)
type RawChange = RawChange' HexInteger Text Text EthAddress EventID

{-
eventID :: RawChange -> EventID
eventID RawChange{logIndex, blockHash} =
  EventID . convert $ Data.Cryptonite.sha256 $ blockHash <> logIndex
  where
    convert :: ByteString -> Text
    convert = undefined
-}

rawChangeTable :: Table RawChangePG RawChangePG
rawChangeTable = table "raw_change"
                       (pRawChange RawChange { logIndex = tableField "log_index"
                                             , transactionHash  = tableField "transaction_hash"
                                             , blockHash = tableField "block_hash"
                                             , address = tableField "address"
                                             , eventID = tableField "event_id"
                                             }
                       )
