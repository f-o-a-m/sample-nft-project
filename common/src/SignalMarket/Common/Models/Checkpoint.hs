{-# LANGUAGE TemplateHaskell #-}

module SignalMarket.Common.Models.Checkpoint where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EventID, HexInteger)

-- | A Checkpoint is an entry into the postgres database noting that an event has been processed.
-- | It's useful for being able to start and stop the indexer to pick up where you left off,
-- | without having to fold over the whole blockchain every time.
data Checkpoint' name bn li status eid = Checkpoint
  { name        :: name
  , blockNumber :: bn
  , logIndex    :: li
  , status      :: status
  , eventID     :: eid
  } deriving (Generic)


$(makeAdaptorAndInstance "pCheckpoint" ''Checkpoint')

type CheckpointPG = Checkpoint' (Field SqlText) (Field SqlNumeric) (Field SqlNumeric) (Field SqlText) (Field SqlText)
type Checkpoint = Checkpoint' Text HexInteger HexInteger Text EventID


checkpointTable :: Table CheckpointPG CheckpointPG
checkpointTable = table "checkpoint"
                       (pCheckpoint Checkpoint { name = tableField "name"
                                               , logIndex = tableField "log_index"
                                               , blockNumber = tableField "block_number"
                                               , status = tableField "status"
                                               , eventID = tableField "event_id"
                                               }
                       )


instance A.ToJSON Checkpoint where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON Checkpoint where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject Checkpoint

instance K.LogItem Checkpoint where
  payloadKeys _ _ = K.AllKeys
