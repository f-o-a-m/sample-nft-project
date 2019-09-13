module SignalMarket.Indexer.Events.FoamToken where

import           SignalMarket.Indexer.Config   (IndexerConfig (..))
import           SignalMarket.Indexer.IndexerM (IndexerM)

{-

Transfer
?Approvals

Requirements:
- insert events into Database
-- connection manager for PG
-- datatype for marshalling/unmarshalling
-- general (HasPG a) => a -> IO (InsertReceipt)

-}

