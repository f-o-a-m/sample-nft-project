module SignalMarket.Server.Server.Signal (signalServer) where

import           Control.Arrow                                        ((>>>))
import qualified Control.Category                                     as Cat
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
                                                                       Value,
                                                                       displayByteNValue,
                                                                       hexIntegerToText)
import qualified SignalMarket.Common.Models.RawChange                 as RawChange
import qualified SignalMarket.Common.Models.Signal                    as Signal
import qualified SignalMarket.Common.Models.SignalTokenTokensUnstaked as SignalTokenTokensUnstaked
import           SignalMarket.Server.API                              (SignalAPI)
import           SignalMarket.Server.API.Types                        (APISignal (..),
                                                                       Cursor (..),
                                                                       WithMetadata (..))
import           SignalMarket.Server.Application                      (AppHandler)
import           SignalMarket.Server.Queries.Combinators              (withCursor,
                                                                       withMetadata)
import           SignalMarket.Server.Queries.Signal                   (hydratedSignalQ,
                                                                       signalByCSTQ,
                                                                       signalByTokenIDQ,
                                                                       signalQ,
                                                                       signalsByCreatorQ,
                                                                       signalsByOwnerQ)
import           SignalMarket.Server.Queries.SignalToken              (signalTokenTokensUnstakedByEIDQ)


signalServer :: ServerT SignalAPI AppHandler
signalServer = getSignalByIDH :<|> getSignalByCSTH :<|> getSignalsByOwnerH :<|> getSignalsByCreatorH :<|> getSignalsByFilterH

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
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> AppHandler [APISignal]
  -- ^ Servant handler which returns fully hydrated signals owned by the specified address
getSignalsByOwnerH owner mlimit moffset =
  let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
   in getSignalInternal (withLimitAndOffset $ signalsByOwnerQ owner)

getSignalsByCreatorH
  :: EthAddress
  -- ^ Address of owner
  -> Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> AppHandler [APISignal]
  -- ^ Servant handler which returns fully hydrated signals created by the specified address
getSignalsByCreatorH creator mlimit moffset =
  let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
   in getSignalInternal (withLimitAndOffset $ signalsByCreatorQ creator)

getSignalsByFilterH
  :: [EthAddress]           -- ^ creators
  -> [EthAddress]           -- ^ owners
  -> [Value]                -- ^ stake-equals
  -> Maybe Value            -- ^ stake-less-than
  -> Maybe Value            -- ^ stake-less-than-or-equals
  -> Maybe Value            -- ^ stake-greater-than
  -> Maybe Value            -- ^ stake-greater-than-or-equals
  -> [Value]                -- ^ radius-equals
  -> Maybe Value            -- ^ radius-less-than
  -> Maybe Value            -- ^ radius-less-than-or-equals
  -> Maybe Value            -- ^ radius-greater-than
  -> Maybe Value            -- ^ radius-greater-than-or-equals
  -> Maybe Int              -- ^ limit
  -> Maybe Int              -- ^ offset
  -> AppHandler [APISignal] -- ^ Servant handler which returns fully hydrated signals matching the filter criteria
getSignalsByFilterH lcreators lowners lstakeEQs mstakeLT mstakeLTE mstakeGT mstakeGTE lradiusEQs mradiusLT mradiusLTE mradiusGT mradiusGTE mlimit moffset =
  let withLimitAndOffset = maybe Cat.id withCursor (Cursor <$> mlimit <*> moffset)
      optionalListFilter mkFilter list =
        case list of
          [] -> Cat.id
          xs -> mkFilter xs
      creatorFilter   = optionalListFilter (\creators -> O.keepWhen (O.in_ (O.constant <$> creators) . Signal.creator)) lcreators
      ownerFilter     = optionalListFilter (\owners -> O.keepWhen (O.in_  (O.constant <$> owners) . Signal.owner)) lowners
      stakeEQsFilter  = optionalListFilter (\stakes -> O.keepWhen (O.in_ (O.constant . Just <$> stakes) . Signal.amountStaked)) lstakeEQs
      radiusEQsFilter = optionalListFilter (\radii  -> O.keepWhen (O.in_ (O.constant . Just <$> radii)  . Signal.radius)) lradiusEQs
      stakeLTFilter   = maybe Cat.id (\stakeLT   -> O.keepWhen (\a -> Signal.amountStaked a O..<  O.constant (Just stakeLT))) mstakeLT
      stakeLTEFilter  = maybe Cat.id (\stakeLTE  -> O.keepWhen (\a -> Signal.amountStaked a O..<= O.constant (Just stakeLTE))) mstakeLTE
      stakeGTFilter   = maybe Cat.id (\stakeGT   -> O.keepWhen (\a -> Signal.amountStaked a O..>  O.constant (Just stakeGT))) mstakeGT
      stakeGTEFilter  = maybe Cat.id (\stakeGTE  -> O.keepWhen (\a -> Signal.amountStaked a O..>= O.constant (Just stakeGTE))) mstakeGTE
      radiusLTFilter  = maybe Cat.id (\radiusLT  -> O.keepWhen (\a -> Signal.radius a O..<  O.constant  (Just radiusLT))) mradiusLT
      radiusLTEFilter = maybe Cat.id (\radiusLTE -> O.keepWhen (\a -> Signal.radius a O..<= O.constant  (Just radiusLTE))) mradiusLTE
      radiusGTFilter  = maybe Cat.id (\radiusGT  -> O.keepWhen (\a -> Signal.radius a O..>  O.constant  (Just radiusGT))) mradiusGT
      radiusGTEFilter = maybe Cat.id (\radiusGTE -> O.keepWhen (\a -> Signal.radius a O..>= O.constant  (Just radiusGTE))) mradiusGTE
   in getSignalInternal $
        withLimitAndOffset $
          signalQ >>> creatorFilter >>> ownerFilter >>>
              stakeEQsFilter  >>> stakeLTFilter  >>> stakeLTEFilter  >>> stakeGTFilter  >>> stakeGTEFilter >>>
              radiusEQsFilter >>> radiusLTFilter >>> radiusLTEFilter >>> radiusGTFilter >>> radiusGTEFilter
