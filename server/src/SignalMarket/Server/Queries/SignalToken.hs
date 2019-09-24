module SignalMarket.Server.Queries.SignalToken where

import qualified Opaleye                                            as O
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as TrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer     as Transfer

signalTokenTransfersQ :: O.Select Transfer.TransferPG
signalTokenTransfersQ = O.selectTable Transfer.transferTable

signalTokenTrackedTokenQ :: O.Select TrackedToken.TrackedTokenPG
signalTokenTrackedTokenQ = O.selectTable TrackedToken.trackedTokenTable
