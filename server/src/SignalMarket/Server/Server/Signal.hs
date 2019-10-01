module SignalMarket.Server.Server.Signal (signalServer) where

--import           Data.String.Conversions                              (cs)
--import qualified Opaleye                                              as O
import           Servant.Server
--import           SignalMarket.Common.Class                            (runDB)
--import           SignalMarket.Common.EventTypes                       (ByteNValue,
--                                                                       EthAddress,
--                                                                       EventID,
--                                                                       TokenID (..),
--                                                                       Value,
--                                                                       displayByteNValue,
--                                                                       hexIntegerToText)
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken as TrackedToken
import           SignalMarket.Server.API                            (SignalAPI)
import           SignalMarket.Server.API.Types                      (APISignal (..),
                                                                     Cursor (..),
                                                                     WithMetadata (..))
import           SignalMarket.Server.Application                    (AppHandler)
import           SignalMarket.Server.Queries.Combinators            (withCursor, withMetadata)


signalServer :: ServerT SignalAPI AppHandler
signalServer = getSignalH

getSignalH
  :: Maybe Int
  -- ^ limit
  -> Maybe Int
  -- ^ offset
  -> AppHandler [WithMetadata TrackedToken.TrackedToken]
getSignalH mlimit moffset = do
  let mCursor = Cursor <$> limit <*> offset
  undefined mCursor
