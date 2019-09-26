module SignalMarket.Server.Queries.SignalToken where

import           Control.Arrow                                        ((>>>))
import qualified Opaleye                                              as O
import           SignalMarket.Common.EventTypes                       (EventID,
                                                                       HexString)
import qualified SignalMarket.Common.Models.RawChange                 as RawChange
import qualified SignalMarket.Common.Models.SignalTokenTokensStaked   as TokensStaked
import qualified SignalMarket.Common.Models.SignalTokenTokensUnstaked as TokensUnstaked
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken   as TrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer       as Transfer
import           SignalMarket.Server.API.Types                        (BlockNumberOrdering (DESC),
                                                                       Cursor (..))
import           SignalMarket.Server.Queries.Combinators              (withCursor,
                                                                       withMetadata,
                                                                       withOrdering)

signalTokenTransfersQ :: O.Select Transfer.TransferPG
signalTokenTransfersQ = O.selectTable Transfer.transferTable

signalTokenTrackedTokenQ :: O.Select TrackedToken.TrackedTokenPG
signalTokenTrackedTokenQ = O.selectTable TrackedToken.trackedTokenTable

signalTokenTokensStakedQ :: O.Select TokensStaked.TokensStakedPG
signalTokenTokensStakedQ = O.selectTable TokensStaked.tokensStakedTable

signalTokenTokensUnstakedQ :: O.Select TokensUnstaked.TokensUnstakedPG
signalTokenTokensUnstakedQ = O.selectTable TokensUnstaked.tokensUnstakedTable

signalTokenTokensUnstakedByEIDQ :: EventID -> O.Select TokensUnstaked.TokensUnstakedPG
signalTokenTokensUnstakedByEIDQ eid =
  let filterByEID = O.keepWhen (\a -> TokensUnstaked.eventID a O..== O.constant eid)
   in (signalTokenTokensUnstakedQ >>> filterByEID)

signalTokenByCSTWithMetadataQ
  :: HexString
  -- ^ CST of signal
  -> O.Select (TrackedToken.TrackedTokenPG, RawChange.RawChangePG)
  -- ^ Opaleye query which gets the TrackedToken event and its associated metadata
signalTokenByCSTWithMetadataQ cst =
  let cstFilter = O.keepWhen (\a -> TrackedToken.cst a O..== O.constant cst)
      limitOne = withCursor (Cursor 1 0)
      orderDesc = withOrdering DESC (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
   in limitOne $
        orderDesc $
          withMetadata TrackedToken.eventID $
            signalTokenTrackedTokenQ >>> cstFilter
