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
import           SignalMarket.Server.Queries.SignalToken        (signalTokenTransfersQ)


signalTokenServer :: ServerT SignalTokenAPI AppHandler
signalTokenServer = getSignalTokenTransfersH

getSignalTokenTransfersH
  :: Maybe EthAddress
  -- ^ to address
  -> Maybe EthAddress
  -- ^ from address
  -> Maybe TokenID
  -- ^ token ID
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -- ^ sort by newest/oldest
  -> AppHandler [WithMetadata SignalTokenTransfer.Transfer]
getSignalTokenTransfersH mto mfrom mtokenid mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        toFilter = maybe Cat.id (\to -> O.keepWhen (\a -> SignalTokenTransfer.to a O..== O.constant to)) mto
        fromFilter = maybe Cat.id (\from -> O.keepWhen (\a -> SignalTokenTransfer.from a O..== O.constant from)) mfrom
        tokenIDFilter = maybe Cat.id (\tokenID -> O.keepWhen (\a -> SignalTokenTransfer.tokenID a O..== O.constant tokenID)) mtokenid
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalTokenTransfer.eventID $
            signalTokenTransfersQ >>> tokenIDFilter >>> toFilter >>> fromFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc
