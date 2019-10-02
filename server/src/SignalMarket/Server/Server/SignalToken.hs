module SignalMarket.Server.Server.SignalToken (signalTokenServer) where

import           Control.Arrow                                  ((>>>))
import qualified Control.Category                               as Cat
import           Data.Maybe                                     (fromMaybe)
import qualified Opaleye                                        as O
import           Servant.Server
import           SignalMarket.Common.Class                      (runDB)
import           SignalMarket.Common.EventTypes                 (EthAddress,
                                                                 TokenID (..))
import qualified SignalMarket.Common.Models.RawChange           as RawChange
import qualified SignalMarket.Common.Models.SignalTokenTransfer as SignalTokenTransfer
import           SignalMarket.Server.API                        (SignalTokenAPI)
import           SignalMarket.Server.API.Types                  (BlockNumberOrdering (..),
                                                                 Cursor (..),
                                                                 WithMetadata (..))
import           SignalMarket.Server.Application                (AppHandler)
import           SignalMarket.Server.Queries.Combinators        (withCursor,
                                                                 withMetadata,
                                                                 withOrdering)
import           SignalMarket.Server.Queries.SignalToken        (signalTokenTransferQ)


signalTokenServer :: ServerT SignalTokenAPI AppHandler
signalTokenServer = getSignalTokenTransfersH

getSignalTokenTransfersH
  :: [EthAddress]
  -- ^ to address
  -> [EthAddress]
  -- ^ from address
  -> [TokenID]
  -- ^ token ID
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -- ^ sort by newest/oldest
  -> AppHandler [WithMetadata SignalTokenTransfer.Transfer]
getSignalTokenTransfersH to from tokenID mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        tokenIdFilter = case tokenID of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalTokenTransfer.tokenID a)
        fromFilter = case from of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalTokenTransfer.from a)
        toFilter = case to of
          [] -> Cat.id
          xs -> O.keepWhen (\a -> fmap O.constant xs `O.in_` SignalTokenTransfer.to a)
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalTokenTransfer.eventID $
            signalTokenTransferQ >>> tokenIdFilter >>> toFilter >>> fromFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc
