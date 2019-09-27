{-# LANGUAGE Arrows #-}
module SignalMarket.Server.Queries.Signal where

import           Control.Arrow                                      (returnA,
                                                                     (>>>))
import qualified Opaleye                                            as O
import           SignalMarket.Common.EventTypes                     (ByteNValue,
                                                                     EthAddress,
                                                                     TokenID)
import qualified SignalMarket.Common.Models.Signal                  as Signal
import qualified SignalMarket.Common.Models.SignalTokenTokensStaked as TokensStaked
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as TrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer     as Transfer
import           SignalMarket.Server.API.Types                      (Cursor (..),
                                                                     WithMetadataPG)
import           SignalMarket.Server.Queries.Combinators            (withCursor, withMetadata)
import           SignalMarket.Server.Queries.SignalToken            as SignalTokenQ

signalQ :: O.Select Signal.SignalPG
signalQ = O.selectTable Signal.signalsTable

signalByTokenIDQ :: TokenID -> O.Select Signal.SignalPG
signalByTokenIDQ tokenID =
  let tokenIDFilter = O.keepWhen (\a -> Signal.tokenID a O..== O.constant tokenID)
      limitOne = withCursor (Cursor 1 0)
   in limitOne $ signalQ >>> tokenIDFilter

signalByCSTQ :: ByteNValue -> O.Select Signal.SignalPG
signalByCSTQ cst =
  let tokenIDFilter = O.keepWhen (\a -> Signal.cst a O..== O.constant (Just cst))
      limitOne = withCursor (Cursor 1 0)
   in limitOne $ signalQ >>> tokenIDFilter

signalsByOwnerQ :: EthAddress -> O.Select Signal.SignalPG
signalsByOwnerQ owner =
  let ownerFilter = O.keepWhen (\a -> Signal.owner a O..== O.constant owner)
   in signalQ >>> ownerFilter

signalsByCreatorQ :: EthAddress -> O.Select Signal.SignalPG
signalsByCreatorQ creator =
  let creatorFilter = O.keepWhen (\a -> Signal.creator a O..== O.constant creator)
   in signalQ >>> creatorFilter

hydratedSignalQ
  :: O.Select Signal.SignalPG                              -- ^ Given a query for a Signal
  -> O.Select ( Signal.SignalPG                            -- ^ Get the results of that query
              , WithMetadataPG TrackedToken.TrackedTokenPG -- ^ Plus its associated TrackedToken event
              , WithMetadataPG TokensStaked.TokensStakedPG -- ^ And its TokenStaked event
              , WithMetadataPG Transfer.TransferPG         -- ^ Ditto for the latest Transfer
              , WithMetadataPG Transfer.TransferPG         -- ^ And the original minting Transfer
              )
hydratedSignalQ getSignal =
  -- We don't want to return signals that haven't been fully indexed, and Opaleye won't let us compare a NULLABLE field to a
  -- non-nullable field. We also want to keep the query simple when dealing with NULL columns, so this is a hack that
  -- tells Opaleye/PG to treat NULLs as a string which can never be an event ID, thereby making the column never match.
  let deNull = O.fromNullable (O.sqlString "_impossible_event_id_")
   in proc () -> do
        signal <- getSignal -< ()

        trackedToken@(tt, _) <- withMetadata TrackedToken.eventID SignalTokenQ.signalTokenTrackedTokenQ   -< ()
        tokensStaked@(ts, _) <- withMetadata TokensStaked.eventID SignalTokenQ.signalTokenTokensStakedQ   -< ()
        lastTransfer@(lt, _) <- withMetadata Transfer.eventID SignalTokenQ.signalTokenTransfersQ          -< ()
        mintTransfer@(mt, _) <- withMetadata Transfer.eventID SignalTokenQ.signalTokenTransfersQ          -< ()

        O.restrict -< TrackedToken.eventID   tt O..== deNull (Signal.trackedTokenEID signal)
        O.restrict -< TokensStaked.eventID   ts O..== deNull (Signal.tokensStakedEID signal)
        O.restrict -< Transfer.eventID       lt O..== Signal.lastTransferEID signal
        O.restrict -< Transfer.eventID       mt O..== Signal.mintingTransferEID signal

        -- note that with these restrictions it will not return anything if any of those joins can't be made
        -- This is OK because that just means the signal hasn't been fully indexed yet.
        -- This is also why we can't return TokensUnstaked here -- it could be legitimately null and therefore not joinable
        -- but would mean that no signal would be returned.
        returnA -< (signal, trackedToken, tokensStaked, lastTransfer, mintTransfer)
