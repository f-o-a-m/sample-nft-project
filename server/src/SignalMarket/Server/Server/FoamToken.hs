module SignalMarket.Server.Server.FoamToken (foamTokenServer) where

import           Control.Arrow                                ((>>>))
import qualified Control.Category                             as Cat
import           Data.Maybe                                   (fromMaybe)
import qualified Opaleye                                      as O
import           Servant.Server
import           SignalMarket.Common.Class                    (runDB)
import           SignalMarket.Common.EventTypes               (EthAddress)
import qualified SignalMarket.Common.Models.FoamTokenTransfer as FoamTokenTransfer
import qualified SignalMarket.Common.Models.RawChange         as RawChange
import           SignalMarket.Server.API                      (FoamTokenAPI)
import           SignalMarket.Server.API.Types                (BlockNumberOrdering (..),
                                                               Cursor (..),
                                                               WithMetadata (..))
import           SignalMarket.Server.Application              (AppHandler)
import           SignalMarket.Server.Queries.Combinators      (withCursor,
                                                               withMetadata,
                                                               withOrdering)
import           SignalMarket.Server.Queries.FoamToken        (foamTokenTransfersQ)


foamTokenServer :: ServerT FoamTokenAPI AppHandler
foamTokenServer = getFoamTokenTransfersH

getFoamTokenTransfersH
  :: [EthAddress]
  -- ^ to address
  -> [EthAddress]
  -- ^ from address
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> Maybe BlockNumberOrdering
  -> AppHandler [WithMetadata FoamTokenTransfer.Transfer]
getFoamTokenTransfersH to from mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        toFilter = case to of
          [] -> Cat.id
          as -> O.keepWhen (\a -> fmap O.constant as `O.in_` FoamTokenTransfer.to a)
        fromFilter = case from of
          [] -> Cat.id
          as -> O.keepWhen (\a -> fmap O.constant as `O.in_` FoamTokenTransfer.from a)
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd) (RawChange.logIndex . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata FoamTokenTransfer.eventID
            $ foamTokenTransfersQ >>> toFilter >>> fromFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc
