{-# LANGUAGE TemplateHaskell #-}

module SignalMarket.Common.Models.RawChange where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlArray, SqlBool,
                                                 SqlNumeric, SqlText, Table,
                                                 table, tableField)
import           SignalMarket.Common.EventTypes (EthAddress, EventID,
                                                 HexInteger, HexString)

-- | 'RawChange' represents the event metadata that comes in with the event.
data RawChange' li ti txh r bh bn addr d ts eid = RawChange
  { logIndex         :: li
  , transactionIndex :: ti
  , transactionHash  :: txh
  , removed          :: r
  , blockHash        :: bh
  , blockNumber      :: bn
  , address          :: addr
  , _data            :: d
  , topics           :: ts
  , eventID          :: eid
  } deriving (Eq, Show, Generic)


$(makeAdaptorAndInstance "pRawChange" ''RawChange')

type RawChangePG = RawChange' (Field SqlNumeric) (Field SqlNumeric) (Field SqlText) (Field SqlBool) (Field SqlText) (Field SqlNumeric) (Field SqlText) (Field SqlText) (Field (SqlArray SqlText)) (Field SqlText)
type RawChange = RawChange' HexInteger HexInteger HexString Bool HexString HexInteger EthAddress HexString [HexString] EventID

rawChangeTable :: Table RawChangePG RawChangePG
rawChangeTable = table "raw_change"
                       (pRawChange RawChange { logIndex = tableField "log_index"
                                             , transactionIndex = tableField "transaction_index"
                                             , transactionHash  = tableField "transaction_hash"
                                             , removed = tableField "removed"
                                             , blockHash = tableField "block_hash"
                                             , blockNumber = tableField "block_number"
                                             , address = tableField "address"
                                             , _data = tableField "data"
                                             , topics = tableField "topics"
                                             , eventID = tableField "event_id"
                                             }
                       )

instance A.ToJSON RawChange where
  toJSON RawChange{..} =
    A.object
      [ "logIndex" A..= logIndex
      , "transactionIndex" A..= transactionIndex
      , "transactionHash" A..= transactionHash
      , "removed" A..= removed
      , "blockHash" A..= blockHash
      , "blockNumber" A..= blockNumber
      , "address" A..= address
      , "data" A..= _data
      , "topics" A..= topics
      , "eventID" A..= eventID
      ]

instance A.FromJSON RawChange where
  parseJSON = A.withObject "RawChange" $ \o ->
    RawChange <$> o A..: "logIndex"
              <*> o A..: "transactionIndex"
              <*> o A..: "transactionHash"
              <*> o A..: "removed"
              <*> o A..: "blockHash"
              <*> o A..: "blockNumber"
              <*> o A..: "address"
              <*> o A..: "data"
              <*> o A..: "topics"
              <*> o A..: "eventID"

instance K.ToObject RawChange

instance K.LogItem RawChange where
  payloadKeys _ _ = K.AllKeys
