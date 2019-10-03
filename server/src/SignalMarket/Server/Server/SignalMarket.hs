module SignalMarket.Server.Server.SignalMarket (signalMarketServer) where

import           Control.Arrow                                         ((<<<),
                                                                        (>>>))
import qualified Control.Category                                      as Cat
import           Control.Monad.Except                                  (throwError)
import           Data.Bifunctor                                        (first)
import           Data.List                                             (sortOn)
import           Data.Maybe                                            (fromMaybe)
import           Data.Ord                                              (Down (..))
import qualified Opaleye                                               as O
import           Servant.API
import           Servant.Server
import           SignalMarket.Common.Class                             (queryMaybe,
                                                                        runDB)
import           SignalMarket.Common.EventTypes                        (EthAddress,
                                                                        HexInteger,
                                                                        SaleID,
                                                                        SaleStatus,
                                                                        TokenID)
import qualified SignalMarket.Common.Models.RawChange                  as RawChange
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale  as ForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold     as Sold
import qualified SignalMarket.Common.Models.SignalMarketSignalUnlisted as Unlisted
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken    as TrackedToken
import           SignalMarket.Server.API                               (SignalMarketAPI)
import           SignalMarket.Server.API.Types                         (BlockNumberOrdering (..),
                                                                        Cursor (..),
                                                                        MarketHistory (..),
                                                                        SignalWithMarketHistoryResponse (..),
                                                                        WithMetadata (..))
import           SignalMarket.Server.Application                       (AppHandler)
import           SignalMarket.Server.Queries.Combinators               (withCursor,
                                                                        withMetadata,
                                                                        withOrdering)
import           SignalMarket.Server.Queries.SignalMarket              (signalMarketSignalForSaleQ,
                                                                        signalMarketSignalSoldQ)

signalMarketServer :: ServerT SignalMarketAPI AppHandler
signalMarketServer = getSignalMarketSignalForSaleH
                :<|> getSignalMarketSignalSoldH
                :<|> getMarketHistoryH

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
  -> AppHandler [WithMetadata ForSale.SignalForSale]
getSignalMarketSignalForSaleH saleID tokenID msaleStatus seller mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        saleIdFilter = case saleID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` ForSale.saleID a)
        tokenIdFilter = case tokenID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` ForSale.tokenID a)
        saleStatusFilter = maybe Cat.id (\x -> O.keepWhen (\a -> ForSale.saleStatus a O..== O.constant x)) msaleStatus
        sellerFilter = case seller of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` ForSale.seller a)
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata ForSale.eventID
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
  -> AppHandler [WithMetadata Sold.SignalSold]
getSignalMarketSignalSoldH saleID tokenID from to mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        saleIdFilter = case saleID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` Sold.saleID a)
        tokenIdFilter = case tokenID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` Sold.tokenID a)
        fromFilter = case from of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` Sold.soldFrom a)
        toFilter = case to of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` Sold.soldTo a)
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata Sold.eventID
            $ signalMarketSignalSoldQ >>> saleIdFilter >>> tokenIdFilter >>> fromFilter >>> toFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc

getMarketHistoryH
  :: TokenID
  -> AppHandler SignalWithMarketHistoryResponse
getMarketHistoryH tokenID = do
  let
    signalTokenQ :: O.Select (TrackedToken.TrackedTokenPG, RawChange.RawChangePG)
    signalTokenQ = withMetadata TrackedToken.eventID $
      O.keepWhen (\a -> TrackedToken.tokenID a O..== O.constant tokenID) <<<
        O.queryTable TrackedToken.trackedTokenTable

    allSoldWithMetaQ :: O.Select (Sold.SignalSoldPG, RawChange.RawChangePG)
    allSoldWithMetaQ = withMetadata Sold.eventID $
      O.keepWhen (\a -> Sold.tokenID a O..== O.constant tokenID) <<<
        O.queryTable Sold.signalSoldTable

    allSalesWithMetaQ :: O.Select (ForSale.SignalForSalePG, RawChange.RawChangePG)
    allSalesWithMetaQ = withMetadata ForSale.eventID $
      O.keepWhen (\a -> ForSale.tokenID a O..== O.constant tokenID) <<<
        O.queryTable ForSale.signalForSaleTable

    allUnlistedWithMetaQ :: O.Select (Unlisted.SignalUnlistedPG, RawChange.RawChangePG)
    allUnlistedWithMetaQ = withMetadata Unlisted.eventID $
      O.keepWhen (\a -> Unlisted.tokenID a O..== O.constant tokenID) <<<
        O.queryTable Unlisted.signalUnlistedTable
  mToken <- queryMaybe $ \conn -> O.runQuery conn signalTokenQ
  case mToken of
    Nothing -> throwError err404
    Just (token, tokenMd) -> do
      history <- runDB $ \conn -> do
        forSale <- map (first ListedForSale) <$> O.runQuery conn allSalesWithMetaQ
        sold <- map (first Sold) <$> O.runQuery conn allSoldWithMetaQ
        unlisted <- map (first Unlisted) <$> O.runQuery conn allUnlistedWithMetaQ
        let sorter :: (MarketHistory, RawChange.RawChange) -> (HexInteger, HexInteger)
            sorter (_, rc) = (RawChange.blockNumber rc, RawChange.logIndex rc)
            sortedHistory = sortOn (Down . sorter) (forSale <> sold <> unlisted)
        pure $ map (\(h,rc) -> WithMetadata h rc) sortedHistory
      pure $ SignalWithMarketHistoryResponse
        { signalWithMarketHistoryResponseSignal = WithMetadata token tokenMd
        , signalWithMarketHistoryResponseHistory = history
        }
