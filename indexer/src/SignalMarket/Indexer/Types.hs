module SignalMarket.Indexer.Types where

import           Network.Ethereum.Api.Types           as W3
import           SignalMarket.Common.EventTypes       (EventID)
import           SignalMarket.Common.Models.RawChange (RawChange)

mkEvent
  :: W3.Change
  -> e
  -> Event e
mkEvent W3.Change{..} e = undefined

data Event e = Event
  { eventEventID  :: EventID
  , eventRawEvent :: RawChange
  , eventData     :: e
  }
