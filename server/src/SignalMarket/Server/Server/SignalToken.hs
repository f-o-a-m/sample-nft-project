module SignalMarket.Server.Server.SignalToken (signalTokenServer) where

import           Control.Arrow                                      ((>>>))
import qualified Control.Category                                   as Cat
import           Control.Monad.Except                               (throwError)
import           Data.Maybe                                         (fromMaybe)
import           Data.String.Conversions                            (cs)
import qualified Opaleye                                            as O
import           Servant                                            ((:<|>) (..))
import           Servant.Server
import           SignalMarket.Common.Class                          (runDB)
import           SignalMarket.Common.EventTypes                     (EthAddress,
                                                                     HexString,
                                                                     TokenID (..),
                                                                     displayHexString,
                                                                     hexIntegerToText)
import qualified SignalMarket.Common.Models.RawChange               as RawChange
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as SignalTokenTrackedToken
import qualified SignalMarket.Common.Models.SignalTokenTransfer     as SignalTokenTransfer
import           SignalMarket.Server.API                            (SignalTokenAPI)
import           SignalMarket.Server.API.Types                      (BlockNumberOrdering (..),
                                                                     Cursor (..),
                                                                     WithMetadata (..))
import           SignalMarket.Server.Application                    (AppHandler)
import           SignalMarket.Server.Queries.Combinators            (withCursor, withMetadata,
                                                                     withOrdering)
import           SignalMarket.Server.Queries.SignalToken            (signalTokenTrackedTokenQ,
                                                                     signalTokenTransfersQ)


signalTokenServer :: ServerT SignalTokenAPI AppHandler
signalTokenServer = getSignalTokenTransfersH :<|> getSignalTokenByIDH :<|> getSignalTokenByCSTH

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
  -> AppHandler [WithMetadata SignalTokenTransfer.Transfer]
getSignalTokenTransfersH mto mfrom mtokenid mlimit moffset mord = do
    let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
        toFilter = maybe Cat.id (\to -> O.keepWhen (\a -> SignalTokenTransfer.to a O..== O.constant to)) mto
        fromFilter = maybe Cat.id (\from -> O.keepWhen (\a -> SignalTokenTransfer.from a O..== O.constant from)) mfrom
        tokenIDFilter = maybe Cat.id (\tokenID -> O.keepWhen (\a -> SignalTokenTransfer.tokenID a O..== O.constant tokenID)) mtokenid
        usingOrdering = withOrdering (fromMaybe DESC mord) (RawChange.blockNumber . snd)
    as <- runDB $ \conn -> O.runQuery conn $
      withLimitAndOffset $
        usingOrdering $
          withMetadata SignalTokenTransfer.eventID $
            signalTokenTransfersQ >>> tokenIDFilter >>> toFilter >>> fromFilter
    pure . flip map as $ \(t, rc) -> WithMetadata t rc

getSignalTokenByIDH
  :: TokenID
  -- ^ nft id of the token
  -> AppHandler (WithMetadata SignalTokenTrackedToken.TrackedToken)
getSignalTokenByIDH tokenID@(TokenID tidHex) = do
  let signalTokenNotFound = err404 { errBody = cs ("No signal with ID " ++ cs (hexIntegerToText tidHex) ++ " exists.") }
      moreThanOneReturned = err500 { errBody = "The impossible happened! Received more than one token despite LIMIT 1!" }
      tokenIDFilter = O.keepWhen (\a -> SignalTokenTrackedToken.tokenID a O..== O.constant tokenID)
      limitOne = withCursor (Cursor 1 0)
      usingOrdering = withOrdering DESC (RawChange.blockNumber . snd)
  as <- runDB $ \conn -> O.runQuery conn $
    limitOne $
      usingOrdering $
        withMetadata SignalTokenTrackedToken.eventID $
          signalTokenTrackedTokenQ >>> tokenIDFilter
  case as of
    [(token, rawChange)] -> pure (WithMetadata token rawChange)
    []                   -> throwError signalTokenNotFound
    _                    -> throwError moreThanOneReturned

getSignalTokenByCSTH
  :: HexString
  -- ^ CST of the signal token
  -> AppHandler (WithMetadata SignalTokenTrackedToken.TrackedToken)
getSignalTokenByCSTH cst = do
  let signalTokenNotFound = err404 { errBody = cs ("No signal with CST " ++ cs (displayHexString cst) ++ " exists.") }
      moreThanOneReturned = err500 { errBody = "The impossible happened! Received more than one token despite LIMIT 1!" }
      cstFilter = O.keepWhen (\a -> SignalTokenTrackedToken.cst a O..== O.constant cst)
      limitOne = withCursor (Cursor 1 0)
      usingOrdering = withOrdering DESC (RawChange.blockNumber . snd)
  as <- runDB $ \conn -> O.runQuery conn $
    limitOne $
      usingOrdering $
        withMetadata SignalTokenTrackedToken.eventID $
          signalTokenTrackedTokenQ >>> cstFilter
  case as of
    [(token, rawChange)] -> pure (WithMetadata token rawChange)
    []                   -> throwError signalTokenNotFound
    _                    -> throwError moreThanOneReturned
