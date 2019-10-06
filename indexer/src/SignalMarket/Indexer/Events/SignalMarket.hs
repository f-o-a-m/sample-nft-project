module SignalMarket.Indexer.Events.SignalMarket where

import           Control.Lens                                          ((^.))
import           Control.Monad.Catch                                   (MonadThrow)
import qualified Katip                                                 as K
import           Opaleye                                               (Column,
                                                                        SqlBool,
                                                                        constant,
                                                                        (.==))
import           SignalMarket.Common.Class                             (MonadPG (..))
import           SignalMarket.Common.Contracts.SignalMarket            as Contract
import           SignalMarket.Common.EventTypes
import           SignalMarket.Common.Models.SignalMarketSignalForSale  as ForSale
import           SignalMarket.Common.Models.SignalMarketSignalSold     as Sold
import           SignalMarket.Common.Models.SignalMarketSignalUnlisted as Unlisted
import           SignalMarket.Indexer.Types
import           SignalMarket.Indexer.Utils                            (insert,
                                                                        update)

-- | insert a new sale on the marketplace into postgres
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
            { ForSale.saleID = signalForSaleSaleId_ ^. _SaleID
            , ForSale.tokenID = signalForSaleTokenId_ ^. _TokenID
            , ForSale.price = signalForSalePrice_ ^. _Value
            , ForSale.saleStatus = SSActive
            , ForSale.seller = signalForSaleSeller_ ^. _EthAddress
            , ForSale.eventID = eventEventID
            }

-- | insert the details of the closed sale into postres, update the
-- | status of the sale to 'complete'.
-- | TODO: use transactions here.
signalMarketSignalSoldH
  :: ( MonadPG m
     , MonadThrow m
     )
  => Event Contract.SignalSold
  -> m ()
signalMarketSignalSoldH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalMarket" $ do
    K.katipAddNamespace "SignalSold" $ do
      case eventData of
        Contract.SignalSold{..} -> do
          -- insert sold event into sold table
          insert Sold.signalSoldTable $ Sold.SignalSold
            { Sold.saleID = signalSoldSaleId_ ^. _SaleID
            , Sold.tokenID = signalSoldTokenId_ ^. _TokenID
            , Sold.price = signalSoldPrice_ ^. _Value
            , Sold.soldFrom = signalSoldOwner_ ^. _EthAddress
            , Sold.soldTo = signalSoldNewOwner_ ^. _EthAddress
            , Sold.eventID = eventEventID
            }
          -- update complete sale status into for sale table
          let updateSaleStatus :: ForSale.SignalForSalePG -> ForSale.SignalForSalePG
              updateSaleStatus a = a { ForSale.saleStatus = constant SSComplete }
              isActiveTokenID :: ForSale.SignalForSalePG -> Column SqlBool
              isActiveTokenID a = ForSale.saleID a .== constant (signalSoldSaleId_ ^. _SaleID)
          _ :: ForSale.SignalForSale <- update ForSale.signalForSaleTable updateSaleStatus isActiveTokenID
          pure ()

-- | insert the details of the unlisted sale into postres, update the
-- | status of the sale to 'unlisted'.
-- | TODO: use transactions here.
signalMarketSignalUnlistedH
  :: ( MonadPG m
     , MonadThrow m
     )
  => Event Contract.SignalUnlisted
  -> m ()
signalMarketSignalUnlistedH Event{eventEventID, eventData} =
  K.katipAddNamespace "SignalMarket" $ do
    K.katipAddNamespace "SignalUnlisted" $ do
      case eventData of
        Contract.SignalUnlisted{..} -> do
          -- insert sold event into sold table
          insert Unlisted.signalUnlistedTable $ Unlisted.SignalUnlisted
            { Unlisted.saleID = signalUnlistedSaleId_ ^. _SaleID
            , Unlisted.tokenID = signalUnlistedTokenId_ ^. _TokenID
            , Unlisted.eventID = eventEventID
            , Unlisted.owner = signalUnlistedOwner_ ^. _EthAddress
            }
          -- update complete sale status into for sale table
          let updateSaleStatus :: ForSale.SignalForSalePG -> ForSale.SignalForSalePG
              updateSaleStatus a = a { ForSale.saleStatus = constant SSUnlisted }
              isActiveTokenID :: ForSale.SignalForSalePG -> Column SqlBool
              isActiveTokenID a = ForSale.saleID a .== constant (signalUnlistedSaleId_ ^. _SaleID)
          _ :: ForSale.SignalForSale <- update ForSale.signalForSaleTable updateSaleStatus isActiveTokenID
          pure ()
