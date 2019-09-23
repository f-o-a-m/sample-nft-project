module SignalMarket.Server.Server.FoamToken (foamTokenServer) where

import           Control.Arrow                                        ((>>>))
import qualified Control.Category                                     as Cat
import           Data.Maybe                                           (fromMaybe)
import qualified Opaleye                                              as O
import           Servant.Server
import           SignalMarket.Common.Class                            (runDB)
import           SignalMarket.Common.EventTypes                       (EthAddress,
                                                                       Value)
import qualified SignalMarket.Common.Models.RawChange                 as RawChange
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as SignalMarketSignalForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold    as SignalMarketSignalSold
import           SignalMarket.Server.API                              (SignalMarketAPI)
import           SignalMarket.Server.API.Types                        (BlockNumberOrdering (..),
                                                                       Cursor (..),
                                                                       WithMetadata (..))
import           SignalMarket.Server.Application                      (AppHandler)
import           SignalMarket.Server.Queries.Combinators              (withCursor,
                                                                       withMetadata,
                                                                       withOrdering)
import           SignalMarket.Server.Queries.SignalMarket             (signalMarketSignalForSaleQ,
                                                                       signalMarketSignalSoldQ)

signalMarketServer :: ServerT SignalMarketAPI AppHandler
signalMarketServer = getSignalMarketSignalForSaleH
                :<|> getSignalMarketSignalSoldH

mkFilter accessF = maybe Cat.id (\x -> O.keepWhen (\a -> accessF a O..== O.constant x))

getSignalMarketSignalForSalesH
  :: Maybe Int
  -- ^ sale_id
  -> Maybe Int
  -- ^ token_id
  -> Maybe Value
  -- ^ price
  -> Maybe SignalMarketSignalForSale.HStatus
  -- ^ sale_status
  -> Maybe EthAddress
  -- ^ seller
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -> AppHandler [WithMetadata SignalMarketSignalForSale.SignalForSale]
getSignalMarketSignalForSalesH msaleID mtokenID mprice msaleStatus mseller mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        saleIdFilter = mkFilter SignalMarketSignalForSale.saleID msaleID
        tokenIdFilter = mkFilter SignalMarketSignalForSale.tokenID mtokenID
        priceFilter = mkFilter SignalMarketSignalForSale.price mprice
        saleStatusFilter = mkFilter SignalMarketSignalForSale.saleStatus msaleStatus
        sellerFilter = mkFilter SignalMarketSignalForSale.seller mseller
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalMarketSignalForSale.eventID
            $ signalMarketServer >>> saleIdFilter >>> tokenIdFilter >>> priceFilter >>> saleStatusFilter >>> sellerFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc

getSignalMarketSignalSoldH
  :: Maybe Int
  -- ^ sale_id
  -> Maybe Int
  -- ^ token_id
  -> Maybe Value
  -- ^ price
  -> Maybe EthAddress
  -- ^ sold_from address
  -> Maybe EthAddress
  -- ^ sold_to address
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -> AppHandler [WithMetadata SignalMarketSignalSold.SignalSold]
getSignalMarketSignalSoldH msaleID mtokenID mprice mfrom mto mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        saleIdFilter = mkFilter SignalMarketSignalSold.saleID msaleID
        tokenIdFilter = mkFilter SignalMarketSignalSold.tokenID mtokenID
        priceFilter = mkFilter SignalMarketSignalSold.price mprice
        fromFilter = mkFilter SignalMarketSignalSold.soldFrom mfrom
        toFilter = mkFilter SignalMarketSignalSold.soldTo mto
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalMarketSignalSold.eventID
            $ signalMarketSignalSoldQ >>> saleIdFilter >>> tokenIdFilter >>> priceFilter >>> saleStatusFilter >>> fromFilter >>> toFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc
