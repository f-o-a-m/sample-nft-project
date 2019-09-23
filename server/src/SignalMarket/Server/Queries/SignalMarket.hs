module SignalMarket.Server.Queries.SignalMarket where

import qualified Opaleye                                              as O
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as ForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold    as Sold
import qualified SignalMarket.Server.API.Types                        as API

signalMarketSignalForSaleQ :: O.Select ForSale.SignalForSalePG
signalMarketSignalForSaleQ = O.selectTable ForSale.signalForSaleTable

signalMarketSignalSoldQ :: O.Select Sold.SignalSoldPG
signalMarketSignalSoldQ = O.selectTable Sold.signalSoldTable
