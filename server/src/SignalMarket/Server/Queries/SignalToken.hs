module SignalMarket.Server.Queries.SignalToken where

import           Control.Arrow                                      (
                                                                     (>>>))
import qualified Opaleye                                            as O
import           SignalMarket.Common.EventTypes                     (ByteNValue,
                                                                     EthAddress,
                                                                     TokenID)
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as TrackedToken
import           SignalMarket.Server.API.Types                      (Cursor (..))
import           SignalMarket.Server.Queries.Combinators            (withCursor)

trackedTokenQ :: O.Select TrackedToken.TrackedTokenPG
trackedTokenQ = O.selectTable TrackedToken.trackedTokenTable

trackedTokenByTokenIDQ :: TokenID -> O.Select TrackedToken.TrackedTokenPG
trackedTokenByTokenIDQ tokenID =
  let tokenIDFilter = O.keepWhen (\a -> TrackedToken.tokenID a O..== O.constant tokenID)
      limitOne = withCursor (Cursor 1 0)
   in limitOne $ trackedTokenQ >>> tokenIDFilter

trackedTokenByCSTQ :: ByteNValue -> O.Select TrackedToken.TrackedTokenPG
trackedTokenByCSTQ cst =
  let tokenIDFilter = O.keepWhen (\a -> TrackedToken.cst a O..== O.constant cst)
      limitOne = withCursor (Cursor 1 0)
   in limitOne $ trackedTokenQ >>> tokenIDFilter

trackedTokensByOwnerQ :: EthAddress -> O.Select TrackedToken.TrackedTokenPG
trackedTokensByOwnerQ owner =
  let ownerFilter = O.keepWhen (\a -> TrackedToken.owner a O..== O.constant owner)
   in trackedTokenQ >>> ownerFilter