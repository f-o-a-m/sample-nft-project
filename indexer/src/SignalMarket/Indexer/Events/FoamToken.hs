module SignalMarket.Indexer.Events.FoamToken where

import SignalMarket.Indexer.IndexerM (IndexerM)
import SignalMarket.Indexer.Config (IndexerConfig(..))

{-

Transfer
?Approvals

Requirements:
- insert events into Database
-- connection manager for PG
-- datatype for marshalling/unmarshalling
-- general (HasPG a) => a -> IO (InsertReceipt)

-}

