module SignalMarket.Server.Queries.SignalToken where

import           Control.Arrow                                        (returnA,
                                                                       (>>>))
import qualified Opaleye                                              as O
import           SignalMarket.Common.EventTypes                       (ByteNValue,
                                                                       EthAddress,
                                                                       SaleID,
                                                                       TokenID,
                                                                       Value)
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as ForSale
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken   as TrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer       as SignalTransfer
import           SignalMarket.Server.API.Types                        (Cursor (..))
import           SignalMarket.Server.Queries.Combinators              (withCursor)
import qualified SignalMarket.Server.Queries.SignalMarket             as SignalMarketQ

signalTokenTransferQ :: O.Select SignalTransfer.TransferPG
signalTokenTransferQ = O.selectTable SignalTransfer.transferTable

type MaybeSaleSummaryPG =
   ( O.Column (O.Nullable O.SqlNumeric) -- saleID
   , O.Column (O.Nullable O.SqlNumeric) -- price
   )
type MaybeSaleSummary = (Maybe SaleID, Maybe Value)

trackedTokenWithSaleQ :: O.Select (TrackedToken.TrackedTokenPG, MaybeSaleSummaryPG)
trackedTokenWithSaleQ =
   let joiner tt sale = (tt , (O.toNullable $ ForSale.saleID sale, O.toNullable $ ForSale.price sale))
       joinerL tt = (tt, (O.null, O.null))
       selector tt sale = TrackedToken.tokenID tt O..== ForSale.tokenID sale
   in proc () -> do
        (t, s) <- O.leftJoinF joiner joinerL selector (O.selectTable TrackedToken.trackedTokenTable) SignalMarketQ.activeSales -< ()
        O.restrict -< TrackedToken.isBurned t O..== O.constant False
        returnA -< (t,s)

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
