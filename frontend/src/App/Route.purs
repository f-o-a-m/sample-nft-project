module App.Route where

import Prelude

import App.Data.SignalId (SignalId, signalIdFromBigNumber, signalIdToBigNumber)
import Control.Error.Util (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Network.Ethereum.Core.BigNumber (decimal)
import Network.Ethereum.Core.BigNumber as BN
import Routing.Duplex (RouteDuplex', as, path, print, root, segment)
import Routing.Duplex.Generic as G

data Route = Signals | Signal SignalId

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ G.sum
  { "Signals": G.noArgs
  , "Signal": path "signal" (signalId segment)
  }

  where

    signalId :: RouteDuplex' String -> RouteDuplex' SignalId
    signalId = as
      (signalIdToBigNumber >>> BN.toString decimal)
      ((BN.parseBigNumber decimal >=> signalIdFromBigNumber) >>> note "Invalid SaleId")

href :: Route -> String
href r = "#" <> print routeCodec r
