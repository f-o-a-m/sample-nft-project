module SignalMarket.Server.Server.Signal (signalServer) where

import           Control.Monad.Except                                 (throwError)
import           Data.Map.Strict                                      (Map)
import qualified Data.Map.Strict                                      as Map
import           Data.Maybe                                           (catMaybes)
import           Data.String.Conversions                              (cs)
import           Data.Traversable                                     (for)
import qualified Opaleye                                              as O
import           Servant                                              ((:<|>) (..))
import           Servant.Server
import           SignalMarket.Common.Class                            (runDB)
import           SignalMarket.Common.EventTypes                       (ByteNValue,
                                                                       EthAddress,
                                                                       EventID,
                                                                       TokenID (..),
                                                                       displayByteNValue,
                                                                       hexIntegerToText)
import qualified SignalMarket.Common.Models.RawChange                 as RawChange
import qualified SignalMarket.Common.Models.Signal                    as Signal
import qualified SignalMarket.Common.Models.SignalTokenTokensUnstaked as SignalTokenTokensUnstaked
import           SignalMarket.Server.API                              (SignalAPI)
import           SignalMarket.Server.API.Types                        (APISignal (..),
                                                                       WithMetadata (..))
import           SignalMarket.Server.Application                      (AppHandler)
import           SignalMarket.Server.Queries.Combinators              (withMetadata)
import           SignalMarket.Server.Queries.Signal                   (hydratedSignalQ,
                                                                       signalByCSTQ,
                                                                       signalByTokenIDQ,
                                                                       signalsByCreatorQ,
                                                                       signalsByOwnerQ)
import           SignalMarket.Server.Queries.SignalToken              (signalTokenTokensUnstakedByEIDQ)


signalServer :: ServerT SignalAPI AppHandler
signalServer = getSignalByIDH :<|> getSignalByCSTH :<|> getSignalsByOwnerH :<|> getSignalsByCreatorH

getSignalInternal
  :: O.Select Signal.SignalPG -- ^ Query that finds the desired signals
  -> AppHandler [APISignal]   -- ^ Servant handler which returns the signals, fully hydrated
getSignalInternal query = do
  let inconsistentData    = err500 { errBody = "The impossible happened! A signal was found with incomplete data!" }
      joinMetadata (a, rawChange) = WithMetadata a rawChange
      shouldExist = maybe (throwError inconsistentData) pure
      pluckSignalRow (signal, _, _, _, _) = signal -- plucks just the signal out of a hydrateSignalQ row
  hydratedSignals <- runDB $ \conn -> O.runQuery conn (hydratedSignalQ query)
  preloadedUnstakeEvents <- preloadUnstakedEvents (pluckSignalRow <$> hydratedSignals)
  for hydratedSignals $ \(signal, trackedToken, tokensStaked, lastTransfer, mintingTransfer) -> do
      let tokenID'   = Signal.tokenID signal
          owner'     = Signal.owner signal
          creator'   = Signal.creator signal

          -- these are here to provide hints to the typechecker as these fields are otherwise never used directly.
          _lastTransferEID   :: EventID       = Signal.lastTransferEID signal
          _mintTransferEID   :: EventID       = Signal.mintingTransferEID signal
          _trackedTokenEID   :: Maybe EventID = Signal.trackedTokenEID signal
          _tokensStakedEID   :: Maybe EventID = Signal.tokensStakedEID signal
      cst'          <- shouldExist (Signal.cst signal)
      geohash'      <- shouldExist (Signal.geohash signal)
      radius'       <- shouldExist (Signal.radius signal)
      amountStaked' <- shouldExist (Signal.amountStaked signal)
      tokensUnstaked' <-
            case Signal.tokensUnstakedEID signal of
              Nothing -> pure Nothing
              Just tuEID -> case Map.lookup tuEID preloadedUnstakeEvents of
                              Nothing -> throwError inconsistentData
                              Just tu -> pure (Just tu)

      pure $ APISignal { tokenID              = tokenID'
                       , owner                = owner'
                       , creator              = creator'
                       , cst                  = cst'
                       , geohash              = geohash'
                       , radius               = radius'
                       , stake                = amountStaked'
                       , trackedTokenEvent    = joinMetadata trackedToken
                       , tokensStakedEvent    = joinMetadata tokensStaked
                       , tokensUnstakedEvent  = tokensUnstaked'
                       , lastTransferEvent    = joinMetadata lastTransfer
                       , mintingTransferEvent = joinMetadata mintingTransfer
                       }

preloadUnstakedEvents
  :: [Signal.Signal]
  -- ^ A list of signal rows
  -> AppHandler (Map EventID (WithMetadata SignalTokenTokensUnstaked.TokensUnstaked))
  -- ^ Servant handler that returns a Mapping of EventID to TokensUnstaked event
preloadUnstakedEvents signals = do
  -- All values of tokensUnstakedEID that aren't NULL in the result set
  let targetEIDS = catMaybes $ Signal.tokensUnstakedEID <$> signals

  -- Read all TokensUnstaked from SQL with those EIDs
  rets <- runDB $ \conn -> O.runQuery conn (withMetadata SignalTokenTokensUnstaked.eventID $ signalTokenTokensUnstakedByEIDQ targetEIDS)

  -- A function that turns a row in the result set into a list of (eventID, WithMetadata event rawChange)
  let asEIDToEventPair (tokensUnstaked, rawChange) = (RawChange.eventID rawChange, WithMetadata tokensUnstaked rawChange)

  -- Apply that function to every row in the list of results
  let asEIDToEventPairs = asEIDToEventPair <$> rets

  -- Create a map that can be easily queried by EventID from that list
  pure (Map.fromList asEIDToEventPairs)


getSignalByIDH
  :: TokenID
  -- ^ nft id of the token
  -> AppHandler APISignal
  -- ^ Servant handler which returns fully hydrated Signal
getSignalByIDH tokenID@(TokenID tidHex) = do
  signals <- getSignalInternal (signalByTokenIDQ tokenID)
  case signals of
    [signal] -> pure signal
    [] -> throwError $ err404 { errBody = cs ("No signal with token ID " ++ cs (hexIntegerToText tidHex) ++ " found!") }
    _  -> throwError $ err500 { errBody = cs ("The impossible happened! More than one signal with token ID " ++ cs (hexIntegerToText tidHex) ++ " was found!") }

getSignalByCSTH
  :: ByteNValue
  -- ^ CST of the signal token
  -> AppHandler APISignal
  -- ^ Servant handler which returns fully hydrated Signal
getSignalByCSTH cst = do
  signals <- getSignalInternal (signalByCSTQ cst)
  case signals of
    [signal] -> pure signal
    [] -> throwError $ err404 { errBody = cs ("No signal with CST " ++ cs (displayByteNValue cst) ++ " found!") }
    _  -> throwError $ err500 { errBody = cs ("The impossible happened! More than one signal with token ID " ++ cs (displayByteNValue cst) ++ " was found!") }

getSignalsByOwnerH
  :: EthAddress
  -- ^ Address of owner
  -> AppHandler [APISignal]
  -- ^ Servant handler which returns fully hydrated signals owned by the specified address
getSignalsByOwnerH owner = getSignalInternal (signalsByOwnerQ owner)

getSignalsByCreatorH
  :: EthAddress
  -- ^ Address of owner
  -> AppHandler [APISignal]
  -- ^ Servant handler which returns fully hydrated signals created by the specified address
getSignalsByCreatorH creator = getSignalInternal (signalsByCreatorQ creator)
