module SignalMarket.Server.Queries.FoamToken where

import qualified Opaleye                                      as O
import qualified SignalMarket.Common.Models.FoamTokenTransfer as Transfer

foamTokenTransfersQ :: O.Select Transfer.TransferPG
foamTokenTransfersQ = O.selectTable Transfer.transferTable
