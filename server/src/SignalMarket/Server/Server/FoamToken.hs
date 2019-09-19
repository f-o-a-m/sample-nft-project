module SignalMarket.Server.Server.FoamToken (foamTokenServer) where

import           Control.Arrow                                ((>>>))
import qualified Control.Category                             as Cat
import qualified Opaleye                                      as O
import           Servant.Server
import           SignalMarket.Common.Class                    (runDB)
import           SignalMarket.Common.EventTypes               (EthAddress)
import qualified SignalMarket.Common.Models.FoamTokenTransfer as FoamTokenTransfer
import qualified SignalMarket.Common.Models.RawChange         as RawChange
import           SignalMarket.Server.API                      (FoamTokenAPI)
import           SignalMarket.Server.API.Types                (Cursor (..),
                                                               WithMetadata (..))
import           SignalMarket.Server.Application              (AppHandler)
import           SignalMarket.Server.Queries.Combinators      (withCursor,
                                                               withMetadata)
import           SignalMarket.Server.Queries.FoamToken        (foamTokenTransfersQ)

foamTokenServer :: ServerT FoamTokenAPI AppHandler
foamTokenServer = getFoamTokenTransfersH

getFoamTokenTransfersH
  :: Maybe EthAddress
  -- ^ to address
  -> Maybe EthAddress
  -- ^ from address
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> AppHandler [WithMetadata FoamTokenTransfer.Transfer]
getFoamTokenTransfersH mto mfrom mlimit moffset = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        toFilter = maybe Cat.id (\to -> O.keepWhen (\a -> FoamTokenTransfer.to a O..== O.constant to)) mto
        fromFilter = maybe Cat.id (\from -> O.keepWhen (\a -> FoamTokenTransfer.from a O..== O.constant from)) mfrom
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        withMetadata FoamTokenTransfer.eventID
          $ foamTokenTransfersQ >>> toFilter >>> fromFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc
