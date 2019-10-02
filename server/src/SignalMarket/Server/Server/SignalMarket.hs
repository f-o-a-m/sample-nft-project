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
                                                                       TokenID)
import qualified SignalMarket.Common.Models.RawChange                 as RawChange
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale as SignalMarketSignalForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold    as SignalMarketSignalSold
import           SignalMarket.Server.API                              (SignalMarketAPI)
import           SignalMarket.Server.API.Types                        (BlockNumberOrdering (..),
                                                                       Cursor (..),
                                                                       SignalWithMarketHistoryResponse (..),
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
                :<|> getMarketHistoryH

-- @TODO: write the type definition of this
-- mkFilter
--   :: (_ -> _)
--   -> Maybe a
--   -> _
-- mkFilter accessF = maybe Cat.id (\x -> O.keepWhen (\a -> accessF a O..== O.constant x))

getSignalMarketSignalForSaleH
  :: [SaleID]
  -- ^ sale_id
  -> [TokenID]
  -- ^ token_id
  -> Maybe SaleStatus
  -- ^ sale_status
  -> [EthAddress]
  -- ^ seller
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -> AppHandler [WithMetadata SignalMarketSignalForSale.SignalForSale]
getSignalMarketSignalForSaleH saleID tokenID msaleStatus seller mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        saleIdFilter = case saleID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalMarketSignalForSale.saleID a)
        tokenIdFilter = case tokenID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalMarketSignalForSale.tokenID a)
        saleStatusFilter = maybe Cat.id (\x -> O.keepWhen (\a -> SignalMarketSignalForSale.saleStatus a O..== O.constant x)) msaleStatus
        sellerFilter = case seller of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalMarketSignalForSale.seller a)
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalMarketSignalForSale.eventID
            $ signalMarketSignalForSaleQ >>> saleIdFilter >>> tokenIdFilter >>> saleStatusFilter >>> sellerFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc

getSignalMarketSignalSoldH
  :: [SaleID]
  -- ^ sale_id
  -> [TokenID]
  -- ^ token_id
  -> [EthAddress]
  -- ^ sold_from address
  -> [EthAddress]
  -- ^ sold_to address
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -> AppHandler [WithMetadata SignalMarketSignalSold.SignalSold]
getSignalMarketSignalSoldH saleID tokenID from to mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        saleIdFilter = case saleID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalMarketSignalSold.saleID a)
        tokenIdFilter = case tokenID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalMarketSignalSold.tokenID a)
        fromFilter = case from of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalMarketSignalSold.soldFrom a)
        toFilter = case to of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalMarketSignalSold.soldTo a)
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalMarketSignalSold.eventID
            $ signalMarketSignalSoldQ >>> saleIdFilter >>> tokenIdFilter >>> fromFilter >>> toFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc

getMarketHistoryH
  :: TokenID
  -> AppHandler SignalWithMarketHistoryResponse
getMarketHistoryH = undefined
