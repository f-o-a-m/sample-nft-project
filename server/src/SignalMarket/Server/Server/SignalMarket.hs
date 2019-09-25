module SignalMarket.Server.Server.SignalMarket (signalMarketServer) where

import           Control.Arrow                                        ((>>>))
import qualified Control.Category                                     as Cat
import           Data.Maybe                                           (fromMaybe)
import qualified Opaleye                                              as O
import           Servant.API
import           Servant.Server
import           SignalMarket.Common.Class                            (runDB)
import           SignalMarket.Common.EventTypes                       (EthAddress,
                                                                       SaleID,
                                                                       SaleStatus,
                                                                       TokenID,
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

-- @TODO: write the type definition of this
-- mkFilter
--   :: (_ -> _)
--   -> Maybe a
--   -> _
-- mkFilter accessF = maybe Cat.id (\x -> O.keepWhen (\a -> accessF a O..== O.constant x))

getSignalMarketSignalForSaleH
  :: Maybe SaleID
  -- ^ sale_id
  -> Maybe TokenID
  -- ^ token_id
  -> Maybe Value
  -- ^ price
  -> Maybe SaleStatus
  -- ^ sale_status
  -> Maybe EthAddress
  -- ^ seller
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -> AppHandler [WithMetadata SignalMarketSignalForSale.SignalForSale]
getSignalMarketSignalForSaleH msaleID mtokenID mprice msaleStatus mseller mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        saleIdFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalForSale.saleID a O..== O.constant x)) msaleID
        tokenIdFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalForSale.tokenID a O..== O.constant x)) mtokenID
        priceFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalForSale.price a O..== O.constant x)) mprice
        saleStatusFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalForSale.saleStatus a O..== O.constant x)) msaleStatus
        sellerFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalForSale.seller a O..== O.constant x)) mseller
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalMarketSignalForSale.eventID
            $ signalMarketSignalForSaleQ >>> saleIdFilter >>> tokenIdFilter >>> priceFilter >>> saleStatusFilter >>> sellerFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc

getSignalMarketSignalSoldH
  :: Maybe SaleID
  -- ^ sale_id
  -> Maybe TokenID
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
        saleIdFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalSold.saleID a O..== O.constant x)) msaleID
        tokenIdFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalSold.tokenID a O..== O.constant x)) mtokenID
        priceFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalSold.price a O..== O.constant x)) mprice
        fromFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalSold.soldFrom a O..== O.constant x)) mfrom
        toFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalSold.soldTo a O..== O.constant x)) mto
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalMarketSignalSold.eventID
            $ signalMarketSignalSoldQ >>> saleIdFilter >>> tokenIdFilter >>> priceFilter >>> fromFilter >>> toFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc
