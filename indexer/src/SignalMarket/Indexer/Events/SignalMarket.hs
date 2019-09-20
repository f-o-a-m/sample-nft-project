module SignalMarket.Indexer.Events.SignalMarket where

import Opaleye ((.==), (.&&), constant, ToFields)
import           Data.Profunctor.Product.Default             (Default)

import           Control.Lens                                         ((^.))
import qualified Katip                                                as K
import           SignalMarket.Common.Class                            (MonadPG (..))
import           SignalMarket.Common.Contracts.SignalMarket           as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.SignalMarketSignalForSale as ForSale
import           SignalMarket.Common.Models.SignalMarketSignalSold    as Sold
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                           (insert, update)

signalMarketSignalForSaleH
  :: MonadPG m
  => Event Contract.SignalForSale
  -> m ()
signalMarketSignalForSaleH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalMarket" $ do
    K.katipAddNamespace "SignalForSale" $ do
      case eventData of
        Contract.SignalForSale{..} ->
          insert ForSale.signalForSaleTable $ ForSale.SignalForSale
            { ForSale.tokenID = signalForSaleSignalId_ ^. _TokenID
            , ForSale.price = signalForSalePrice_ ^. _Value
            , ForSale.saleStatus = ForSale.HActive
            , ForSale.eventID = eventEventID
            }

signalMarketSignalSoldH
  :: MonadPG m
  -- => Default ToFields f h 
  => Event Contract.SignalSold
  -> m ()
signalMarketSignalSoldH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalMarket" $ do
    K.katipAddNamespace "SignalSold" $ do
      case eventData of
        Contract.SignalSold{..} -> do
          -- insert sold event into sold table
          insert Sold.signalSoldTable $ Sold.SignalSold
            { Sold.tokenID = signalSoldSignalId_ ^. _TokenID
            , Sold.price = signalSoldPrice_ ^. _Value
            , Sold.soldFrom = signalSoldOwner_ ^. _EthAddress
            , Sold.soldTo = signalSoldNewOwner_ ^. _EthAddress
            , Sold.eventID = eventEventID
            }
            
          let updateSaleStatus a = a { ForSale.saleStatus = constant ForSale.HComplete }
              isActiveTokenID a =
                ForSale.tokenID a .== constant (signalSoldSignalId_ ^. _TokenID) .&&
                ForSale.saleStatus a .== constant ForSale.HActive
          -- update complete sale status into for sale table
          update ForSale.signalForSaleTable updateSaleStatus isActiveTokenID
          pure ()
