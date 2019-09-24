module SignalMarket.Indexer.Events.SignalToken where

import           Control.Lens                                       ((^.))
import qualified Katip                                              as K
import           SignalMarket.Common.Class                          (MonadPG (..))
import           SignalMarket.Common.Contracts.SignalToken          as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.SignalTokenTrackedToken as TrackedToken
import           SignalMarket.Common.Models.SignalTokenTransfer     as Transfer
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                         (insert)

signalTokenTransferH
  :: MonadPG m
  => Event Contract.Transfer
  -> m ()
signalTokenTransferH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalToken" $ do
    K.katipAddNamespace "Transfer" $ do
      case eventData of
        Contract.Transfer{..} ->
          insert Transfer.transferTable $ Transfer.Transfer
            { Transfer.to = transferTo_ ^. _EthAddress
            , Transfer.from = transferFrom_ ^. _EthAddress
            , Transfer.tokenID = transferTokenId_ ^. _TokenID
            , Transfer.eventID = eventEventID
            }

signalTokenTrackedTokenH
  :: MonadPG m
  => Event Contract.TrackedToken
  -> m ()
signalTokenTrackedTokenH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalToken" $ do
    K.katipAddNamespace "TrackedToken" $ do
      case eventData of
        Contract.TrackedToken{..} ->
          insert TrackedToken.trackedTokenTable $ TrackedToken.TrackedToken
            { TrackedToken.nftAddress = trackedTokenNftAddress_ ^. _EthAddress
            , TrackedToken.cst = trackedTokenCst_ ^. _HexBytesN
            , TrackedToken.geohash = trackedTokenGeohash_ ^. _HexBytesN
            , TrackedToken.radius = trackedTokenRadius_ ^. _Value
            , TrackedToken.tokenID = trackedTokenTokenID_ ^. _TokenID
            , TrackedToken.eventID = eventEventID
            }
