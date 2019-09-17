module SignalMarket.Indexer.Events.SignalToken where

import           Control.Lens                                   ((^.))
import qualified Katip                                          as K
import           SignalMarket.Common.Contracts.SignalToken      as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.SignalTokenTransfer as Model
import           SignalMarket.Indexer.Class                     (MonadPG (..))
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                     (insert)

signalTokenTransferH
  :: MonadPG m
  => Event Contract.Transfer
  -> m ()
signalTokenTransferH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalToken" $ do
    K.katipAddNamespace "Transfer" $ do
      case eventData of
        Contract.Transfer{..} ->
          insert Model.transferTable $ Model.Transfer
            { Model.to = transferTo_ ^. _EthAddress
            , Model.from = transferFrom_ ^. _EthAddress
            , Model.tokenID = transferValue_ ^. _TokenID
            , Model.eventID = eventEventID
            }
