module SignalMarket.Server.Queries.SignalMarket where

import           Control.Arrow                                        (returnA)
import qualified Opaleye                                              as O
import           SignalMarket.Common.EventTypes
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as ForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold    as Sold

signalMarketSignalForSaleQ :: O.Select ForSale.SignalForSalePG
signalMarketSignalForSaleQ = O.selectTable ForSale.signalForSaleTable

signalMarketSignalSoldQ :: O.Select Sold.SignalSoldPG
signalMarketSignalSoldQ = O.selectTable Sold.signalSoldTable

activeSales :: O.Select ForSale.SignalForSalePG
activeSales = proc () -> do
    sig <- O.queryTable ForSale.signalForSaleTable -< ()
    O.restrict -< ForSale.saleStatus sig O..== O.constant SSActive
    returnA -< sig
