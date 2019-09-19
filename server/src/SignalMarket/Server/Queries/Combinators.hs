module SignalMarket.Server.Queries.Combinators where

import qualified Opaleye as O
import qualified SignalMarket.Server.API.Types as API
import qualified SignalMarket.Common.Models.RawChange as RawChange

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