module SignalMarket.Indexer.Events.FoamToken where

import           Control.Monad.Reader                         (asks)
import           Data.Default                           (def)
import           Data.Proxy
import qualified Katip                                        as K
import           Network.Ethereum.Api.Provider                (runWeb3With)
import           Network.Ethereum.Api.Types                   (Filter (..))
import           Network.Ethereum.Contract.Event.Common       (EventAction (..))
import           Network.Ethereum.Contract.Event.SingleFilter (event')
import           SignalMarket.Common.Config.Types             (contractsFoamToken, deployReceiptAddress)
import           SignalMarket.Common.Contracts.FoamToken      as Contract
import           SignalMarket.Common.Models.FoamTokenTransfer as Model
import           SignalMarket.Indexer.Config                  (IndexerConfig (..))
import           SignalMarket.Indexer.IndexerM                (IndexerM)
import SignalMarket.Indexer.Utils (insert)
import Data.String (fromString)
import Control.Monad.IO.Class (liftIO)
import SignalMarket.Common.EventTypes

foamTokenTransferMonitor :: IndexerM ()
foamTokenTransferMonitor = do
  K.katipAddNamespace "FoamToken" $ do
    K.katipAddNamespace "Transfer" $ do
      foamTokenAddress <- asks (deployReceiptAddress . contractsFoamToken . indexerCfgContracts)
      let flter = (def @(Filter Contract.Transfer)) {filterAddress = Just [foamTokenAddress]}
      conn <- asks indexerPGConnection
      (provider, manager) <- asks indexerCfgWeb3Manager
      eRes <- runWeb3With manager provider $
        event' flter $ \Contract.Transfer{..} -> do
          _ <- liftIO $ insert conn Model.transferTable $ Model.Transfer
            { Model.to = (undefined :: EthAddress)
            , Model.from = (undefined :: EthAddress)
            , Model.value = (undefined  :: Value)
            , Model.eventID = (undefined :: EventID)
            }
          pure $ ContinueEvent
      case eRes of
          Left e ->
              K.logFM K.ErrorS $ fromString ("Error in event monitor: " <> show e)
          Right _ -> pure ()



