module SignalMarket.Server.Queries.SignalToken where

import           Control.Arrow                                        ((>>>))
import qualified Opaleye                                              as O
import           SignalMarket.Common.EventTypes                       (EventID)
import qualified SignalMarket.Common.Models.SignalTokenTokensStaked   as TokensStaked
import qualified SignalMarket.Common.Models.SignalTokenTokensUnstaked as TokensUnstaked
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken   as TrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer       as Transfer

signalTokenTransfersQ :: O.Select Transfer.TransferPG
signalTokenTransfersQ = O.selectTable Transfer.transferTable

signalTokenTrackedTokenQ :: O.Select TrackedToken.TrackedTokenPG
signalTokenTrackedTokenQ = O.selectTable TrackedToken.trackedTokenTable

signalTokenTokensStakedQ :: O.Select TokensStaked.TokensStakedPG
signalTokenTokensStakedQ = O.selectTable TokensStaked.tokensStakedTable

signalTokenTokensUnstakedQ :: O.Select TokensUnstaked.TokensUnstakedPG
signalTokenTokensUnstakedQ = O.selectTable TokensUnstaked.tokensUnstakedTable

signalTokenTokensUnstakedByEIDQ :: [EventID] -> O.Select TokensUnstaked.TokensUnstakedPG
signalTokenTokensUnstakedByEIDQ eids =
  let targetEIDs = map O.constant eids
      filterByEIDs = O.keepWhen (O.in_ targetEIDs . TokensUnstaked.eventID)
   in (signalTokenTokensUnstakedQ >>> filterByEIDs)
