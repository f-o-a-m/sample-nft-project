module SignalMarket.Indexer.Events.FoamToken where

import           Control.Monad.IO.Class                       (liftIO)
import qualified Katip                                        as K
import           SignalMarket.Common.Contracts.FoamToken      as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.FoamTokenTransfer as Model
import           SignalMarket.Indexer.Class                   (MonadPG (..))
import           SignalMarket.Indexer.Config
import           SignalMarket.Indexer.IndexerM                (IndexerM)
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                   (insert)

foamTokenTransferH
  :: MonadPG m
  => Event Contract.Transfer
  -> m ()
foamTokenTransferH Event{eventEventID, eventData} = do
  K.katipAddNamespace "FoamToken" $ do
    K.katipAddNamespace "Transfer" $ do
      insert Model.transferTable $ Model.Transfer
        { Model.to = undefined :: EthAddress
        , Model.from = undefined :: EthAddress
        , Model.value = undefined  :: Value
        , Model.eventID = undefined :: EventID
        }
