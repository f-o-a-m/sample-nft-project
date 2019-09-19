module SignalMarket.Server.Server.FoamToken where

import Control.Arrow ((>>>))
import SignalMarket.Server.Queries.FoamToken (foamTokenTransfersQ)
import SignalMarket.Server.Queries.Combinators (withCursor, withMetadata)
import SignalMarket.Server.Application (AppHandler)
import qualified SignalMarket.Common.Models.FoamTokenTransfer as FoamTokenTransfer
import qualified SignalMarket.Common.Models.RawChange as RawChange
import SignalMarket.Common.EventTypes (EthAddress)
import SignalMarket.Common.Class (runDB)
import Servant.Server
import SignalMarket.Server.API (FoamTokenAPI)
import SignalMarket.Server.API.Types (WithMetadata(..), Cursor(..))
import qualified Opaleye as O

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
    let mCursor = Cursor <$> mlimit <*> moffset
        toFilter = maybe id (\to -> O.keepWhen (\a -> FoamTokenTransfer.to O..== to)) mto
        fromFilter = maybe id (\from -> O.keepWhen (\a -> FoamTokenTransfer.from a O..== from)) mfrom
    as :: [(FoamTokenTransfer.Transfer, RawChange.RawChange)] <- runDB $ \conn -> O.runQuery conn $ 
      withCursor $ 
        withMetadata FoamTokenTransfer.eventID 
          $ foamTokenTransfersQ >>> toFilter >>> fromFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc