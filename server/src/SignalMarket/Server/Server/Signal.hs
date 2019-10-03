module SignalMarket.Server.Server.Signal (signalServer) where

import           Control.Arrow                                      ((>>>))
import qualified Control.Category                                   as Cat
import qualified Opaleye                                            as O
import           Servant.Server
import           SignalMarket.Common.Class                          (runDB)
import           SignalMarket.Common.EventTypes                     (EthAddress,
                                                                     TokenID)

import qualified SignalMarket.Common.Models.RawChange               as RC
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as TrackedToken
import           SignalMarket.Server.API                            (SignalAPI)
import           SignalMarket.Server.API.Types                      (Cursor (..),
                                                                     SaleSummary (..),
                                                                     SignalWithSaleResponse,
                                                                     SignalWithSaleSummary (..),
                                                                     WithMetadata (..))
import           SignalMarket.Server.Application                    (AppHandler)
import           SignalMarket.Server.Queries.Combinators            (withCursor, withMetadata)
import           SignalMarket.Server.Queries.SignalToken            (MaybeSaleSummary,
                                                                     trackedTokenWithSaleQ)


signalServer :: ServerT SignalAPI AppHandler
signalServer = getSignalH

getSignalH
  :: Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> [EthAddress]
  -> [TokenID]
  -> AppHandler SignalWithSaleResponse
getSignalH mlimit moffset owners tokens = do
  let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
      ownersF = case owners of
        [] -> Cat.id
        as -> O.keepWhen $ \a -> fmap O.constant as `O.in_` TrackedToken.owner (fst a)
      tokenF = case tokens of
        [] -> Cat.id
        as -> O.keepWhen $ \a -> fmap O.constant as `O.in_` TrackedToken.tokenID (fst a)
  as :: [((TrackedToken.TrackedToken, MaybeSaleSummary), RC.RawChange)] <- runDB $ \conn -> O.runQuery conn $
    withLimitAndOffset $
      withMetadata (\(tt, _) -> TrackedToken.eventID tt) $
          trackedTokenWithSaleQ >>> ownersF >>> tokenF
  pure . flip map as $ \((t,ss), rc) ->
    let saleSummary = SaleSummary <$> fst ss <*> snd ss
        apiSignal = SignalWithSaleSummary
          { signalWithSaleResponseSignal = t
          , signalWithSaleResponseSale = saleSummary
          }
    in WithMetadata apiSignal rc
