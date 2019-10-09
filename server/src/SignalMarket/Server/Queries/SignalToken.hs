module SignalMarket.Server.Queries.SignalToken where

import           Control.Arrow                                        (returnA)
import qualified Opaleye                                              as O
import           SignalMarket.Common.EventTypes                       (EthAddress,
                                                                       SaleID,
                                                                       Value)
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as ForSale
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken   as TrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer       as SignalTransfer
import qualified SignalMarket.Server.Queries.SignalMarket             as SignalMarketQ

signalTokenTransferQ :: O.Select SignalTransfer.TransferPG
signalTokenTransferQ = O.selectTable SignalTransfer.transferTable

type MaybeSaleSummaryPG =
   ( O.Column (O.Nullable O.SqlNumeric) -- saleID
   , O.Column (O.Nullable O.SqlNumeric) -- price
   , O.Column (O.Nullable O.SqlText) -- owner
   )
type MaybeSaleSummary = (Maybe SaleID, Maybe Value, Maybe EthAddress)

trackedTokenWithSaleQ :: O.Select (TrackedToken.TrackedTokenPG, MaybeSaleSummaryPG)
trackedTokenWithSaleQ =
   let joiner tt sale = (tt , (O.toNullable $ ForSale.saleID sale, O.toNullable $ ForSale.price sale, O.toNullable $ ForSale.seller sale))
       joinerL tt = (tt, (O.null, O.null, O.null))
       selector tt sale = TrackedToken.tokenID tt O..== ForSale.tokenID sale
   in proc () -> do
        (t, s) <- O.leftJoinF joiner joinerL selector (O.selectTable TrackedToken.trackedTokenTable) SignalMarketQ.activeSales -< ()
        O.restrict -< TrackedToken.isBurned t O..== O.constant False
        returnA -< (t,s)

trackedTokenQ :: O.Select TrackedToken.TrackedTokenPG
trackedTokenQ = O.selectTable TrackedToken.trackedTokenTable
