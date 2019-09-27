module SignalMarket.Indexer.Events.SignalToken where

import           Control.Lens                                         ((^.))
import           Control.Monad.Catch                                  (MonadThrow)
import qualified Katip                                                as K
import           Opaleye                                              (constant,
                                                                       (.==))
import           SignalMarket.Common.Class                            (MonadPG (..))
import           SignalMarket.Common.Contracts.SignalToken            as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.Signal                    as Signal
import           SignalMarket.Common.Models.SignalTokenTokensStaked   as TokensStaked
import           SignalMarket.Common.Models.SignalTokenTokensUnstaked as TokensUnstaked
import           SignalMarket.Common.Models.SignalTokenTrackedToken   as TrackedToken
import           SignalMarket.Common.Models.SignalTokenTransfer       as Transfer
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                           (insert,
                                                                       update)

signalTokenTransferH
  :: ( MonadPG m
     , MonadThrow m
     )
  => Event Contract.Transfer
  -> m ()
signalTokenTransferH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalToken" $ do
    K.katipAddNamespace "Transfer" $ do
      case eventData of
        Contract.Transfer{..} -> do
          let tokenID            = transferTokenId_ ^. _TokenID
              originalTokenOwner = transferFrom_ ^. _EthAddress
              newTokenOwner      = transferTo_ ^. _EthAddress
              isMinted           = originalTokenOwner == zeroAddress
          insert Transfer.transferTable $ Transfer.Transfer
            { Transfer.to = newTokenOwner
            , Transfer.from = originalTokenOwner
            , Transfer.tokenID = tokenID
            , Transfer.eventID = eventEventID
            }
          if isMinted
          then
            insert Signal.signalsTable $ Signal.Signal
              { Signal.tokenID = tokenID
              , Signal.owner = newTokenOwner
              , Signal.creator = newTokenOwner
              , Signal.amountStaked = Nothing :: Maybe Value -- gets filled in by TokensStaked event
              , Signal.cst = Nothing :: Maybe ByteNValue
              , Signal.geohash = Nothing :: Maybe ByteNValue
              , Signal.radius = Nothing :: Maybe Value
              , Signal.trackedTokenEID = Nothing :: Maybe EventID
              , Signal.tokensStakedEID = Nothing :: Maybe EventID
              , Signal.tokensUnstakedEID = Nothing :: Maybe EventID
              , Signal.lastTransferEID = eventEventID
              , Signal.mintingTransferEID = eventEventID
              }
          else do
            let updateSignalOwner s = s { Signal.owner = constant newTokenOwner }
                isTargetSignalID  s = Signal.tokenID s .== constant tokenID
            _ :: Signal.Signal <- update Signal.signalsTable updateSignalOwner isTargetSignalID
            pure ()

signalTokenTrackedTokenH
  :: ( MonadPG m
     , MonadThrow m
     )
  => Event Contract.TrackedToken
  -> m ()
signalTokenTrackedTokenH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalToken" $ do
    K.katipAddNamespace "TrackedToken" $ do
      case eventData of
        Contract.TrackedToken{..} -> do
          let tokenID = trackedTokenTokenID_ ^. _TokenID
              cst     = trackedTokenCst_ ^. _HexBytesN
              geohash = trackedTokenGeohash_ ^. _HexBytesN
              radius  = trackedTokenRadius_ ^. _Value
              updateSignal s = s { Signal.trackedTokenEID = constant (Just eventEventID)
                                 , Signal.cst = constant (Just cst)
                                 , Signal.geohash = constant (Just geohash)
                                 , Signal.radius = constant (Just radius)
                                 }
              isTargetSignalID s = Signal.tokenID s .== constant tokenID
          insert TrackedToken.trackedTokenTable $ TrackedToken.TrackedToken
            { TrackedToken.nftAddress = trackedTokenNftAddress_ ^. _EthAddress
            , TrackedToken.cst = cst
            , TrackedToken.geohash = geohash
            , TrackedToken.radius = radius
            , TrackedToken.tokenID = tokenID
            , TrackedToken.eventID = eventEventID
            }
          _ :: Signal.Signal <- update Signal.signalsTable updateSignal isTargetSignalID
          pure ()

signalTokenTokensStakedH
  :: ( MonadPG m
     , MonadThrow m
     )
  => Event Contract.TokensStaked
  -> m ()
signalTokenTokensStakedH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalToken" $ do
    K.katipAddNamespace "TokensStaked" $ do
      case eventData of
        Contract.TokensStaked{..} -> do
          let tokenID = tokensStakedTokenID_ ^. _TokenID
              stakeAmount = tokensStakedTokensStaked_ ^. _Value
              updateSignal s = s { Signal.tokensStakedEID = constant (Just eventEventID), Signal.amountStaked = constant (Just stakeAmount) }
              isTargetSignalID s = Signal.tokenID s .== constant tokenID
          insert TokensStaked.tokensStakedTable $ TokensStaked.TokensStaked
            { TokensStaked.tokenID = tokenID
            , TokensStaked.staker = tokensStakedStaker_ ^. _EthAddress
            , TokensStaked.stakeAmount = stakeAmount
            , TokensStaked.timestamp = tokensStakedStakeTimestamp_ ^. _Value
            , TokensStaked.eventID = eventEventID
            }
          _ :: Signal.Signal <- update Signal.signalsTable updateSignal isTargetSignalID
          pure ()

signalTokenTokensUnstakedH
  :: ( MonadPG m
     , MonadThrow m
     )
  => Event Contract.TokensUnstaked
  -> m ()
signalTokenTokensUnstakedH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalToken" $ do
    K.katipAddNamespace "TokensUnstaked" $ do
      case eventData of
        Contract.TokensUnstaked{..} -> do
          let tokenID = tokensUnstakedTokenID_ ^. _TokenID
              updateSignal s = s { Signal.tokensUnstakedEID = constant (Just eventEventID), Signal.amountStaked = constant (Just 0 :: Maybe Value) }
              isTargetSignalID s = Signal.tokenID s .== constant tokenID
          insert TokensUnstaked.tokensUnstakedTable $ TokensUnstaked.TokensUnstaked
            { TokensUnstaked.tokenID = tokenID
            , TokensUnstaked.staker = tokensUnstakedStaker_ ^. _EthAddress
            , TokensUnstaked.stakeAmount = tokensUnstakedTokensUnstaked_ ^. _Value
            , TokensUnstaked.timestamp = tokensUnstakedUnstakeTimestamp_ ^. _Value
            , TokensUnstaked.eventID = eventEventID
            }
          _ :: Signal.Signal <- update Signal.signalsTable updateSignal isTargetSignalID
          pure ()
