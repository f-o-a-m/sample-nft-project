module SignalMarket.Server.Queries.SignalToken where

import           Control.Arrow                                      ((>>>))
import qualified Opaleye                                            as O
import           SignalMarket.Common.EventTypes                     (HexString,
                                                                     TokenID)
import qualified SignalMarket.Common.Models.RawChange               as RawChange
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as TrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer     as Transfer
import           SignalMarket.Server.API.Types                      (BlockNumberOrdering (DESC),
                                                                     Cursor (..))
import           SignalMarket.Server.Queries.Combinators            (withCursor, withMetadata,
                                                                     withOrdering)

signalTokenTransfersQ :: O.Select Transfer.TransferPG
signalTokenTransfersQ = O.selectTable Transfer.transferTable

latestSignalTokenTransferQ
  :: TokenID
  -- ^ NFT Token ID of token to get latest transfer event of
  -> O.Select (Transfer.TransferPG, RawChange.RawChangePG)
  -- ^ Opaleye query which gets the latest transfer event and its associated metadata
latestSignalTokenTransferQ tokenID =
  let limitOne      = withCursor (Cursor 1 0)
      orderDesc     = withOrdering DESC (RawChange.blockNumber . snd)
      tokenIDFilter = O.keepWhen (\a -> Transfer.tokenID a O..== O.constant tokenID)
   in limitOne $
        orderDesc $
          withMetadata Transfer.eventID $
            signalTokenTransfersQ >>> tokenIDFilter


signalTokenTrackedTokenQ :: O.Select TrackedToken.TrackedTokenPG
signalTokenTrackedTokenQ = O.selectTable TrackedToken.trackedTokenTable

signalTokenByIDWithMetadataQ
  :: TokenID
  -- ^ NFT token ID of the signal
  -> O.Select (TrackedToken.TrackedTokenPG, RawChange.RawChangePG)
  -- ^ Opaleye query which gets the TrackedToken event and its associated metadata
signalTokenByIDWithMetadataQ tokenID =
  let tokenIDFilter = O.keepWhen (\a -> TrackedToken.tokenID a O..== O.constant tokenID)
      limitOne = withCursor (Cursor 1 0)
      orderDesc = withOrdering DESC (RawChange.blockNumber . snd)
   in limitOne $
        orderDesc $
          withMetadata TrackedToken.eventID $
            signalTokenTrackedTokenQ >>> tokenIDFilter

signalTokenByCSTWithMetadataQ
  :: HexString
  -- ^ CST of signal
  -> O.Select (TrackedToken.TrackedTokenPG, RawChange.RawChangePG)
  -- ^ Opaleye query which gets the TrackedToken event and its associated metadata
signalTokenByCSTWithMetadataQ cst =
  let cstFilter = O.keepWhen (\a -> TrackedToken.cst a O..== O.constant cst)
      limitOne = withCursor (Cursor 1 0)
      orderDesc = withOrdering DESC (RawChange.blockNumber . snd)
   in limitOne $
        orderDesc $
          withMetadata TrackedToken.eventID $
            signalTokenTrackedTokenQ >>> cstFilter
