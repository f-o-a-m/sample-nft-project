module SignalMarket.Indexer.Events.SignalMarket where

import           Control.Lens                                         ((^.))
import qualified Katip                                                as K
import           SignalMarket.Common.Contracts.SignalMarket           as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.SignalMarketSignalForSale as ForSale
import           SignalMarket.Common.Models.SignalMarketSignalSold    as Sold
import           SignalMarket.Indexer.Class                           (MonadPG (..))
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                           (insert)

signalMarketSignalForSaleH
  :: MonadPG m
  => Event Contract.SignalForSale
  -> m ()
signalMarketSignalForSaleH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalMarkt" $ do
    K.katipAddNamespace "SignalForSale" $ do
      case eventData of
        Contract.SignalForSale{..} ->
          insert ForSale.signalForSaleTable $ ForSale.SignalForSale
            { ForSale.tokenID = signalForSaleSignalId_ ^. _TokenID
            , ForSale.price = signalForSalePrice_ ^. _Value
            , ForSale.eventID = eventEventID
            }

signalMarketSignalSoldH
  :: MonadPG m
  => Event Contract.SignalSold
  -> m ()
signalMarketSignalSoldH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalMarkt" $ do
    K.katipAddNamespace "SignalSold" $ do
      case eventData of
        Contract.SignalSold{..} ->
          insert Sold.signalSoldTable $ Sold.SignalSold
            { Sold.tokenID = signalSoldSignalId_ ^. _TokenID
            , Sold.price = signalSoldPrice_ ^. _Value
            , Sold.soldFrom = signalSoldOwner_ ^. _EthAddress
            , Sold.soldTo = signalSoldNewOwner_ ^. _EthAddress
            , Sold.eventID = eventEventID
            }
