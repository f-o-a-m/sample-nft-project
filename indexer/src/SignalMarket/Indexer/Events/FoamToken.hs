module SignalMarket.Indexer.Events.FoamToken where

import           Control.Lens                                 ((^.))
import qualified Katip                                        as K
import           SignalMarket.Common.Class                    (MonadPG (..))
import           SignalMarket.Common.Contracts.FoamToken      as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.FoamTokenTransfer as Model
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                   (insert)

foamTokenTransferH
  :: MonadPG m
  => Event Contract.Transfer
  -> m ()
foamTokenTransferH Event{eventEventID, eventData} =
  K.katipAddNamespace "FoamToken" $ do
    K.katipAddNamespace "Transfer" $ do
      case eventData of
        Contract.Transfer{..} ->
          insert Model.transferTable $ Model.Transfer
            { Model.to = transferTo_ ^. _EthAddress
            , Model.from = transferFrom_ ^. _EthAddress
            , Model.value = transferValue_ ^. _Value
            , Model.eventID = eventEventID
            }
