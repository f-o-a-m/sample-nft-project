module SignalMarket.Server.Queries.Combinators where

import qualified Opaleye                              as O
import qualified SignalMarket.Common.Models.RawChange as RawChange
import qualified SignalMarket.Server.API.Types        as API

withCursor
  :: API.Cursor
  -> O.Select a
  -> O.Select a
withCursor API.Cursor{..} =
  O.limit limit . O.offset offset

withMetadata
  :: (a -> O.Column O.SqlText)
  -> O.Select a
  -> O.Select (a, RawChange.RawChangePG)
withMetadata getEventID q =
    let joiner = (,)
        selector a rc = getEventID a O..== RawChange.eventID rc
    in O.joinF joiner selector q (O.selectTable  RawChange.rawChangeTable)

withOrdering
  :: API.BlockNumberOrdering
  -> (a -> O.Column O.SqlNumeric)
  -> (a -> O.Column O.SqlNumeric)
  -> O.Select a
  -> O.Select a
withOrdering ord getBlockNumber getLogIndex =
    let ordering = case ord of
            API.ASC  -> O.asc getBlockNumber <> O.asc getLogIndex
            API.DESC -> O.desc getBlockNumber <> O.desc getLogIndex
    in O.orderBy ordering
