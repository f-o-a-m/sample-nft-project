module App.Route where

import Prelude

import App.Data.SignalID (SignalID)
import App.Data.SignalID as SignalID
import Control.Error.Util (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', as, path, root, segment)
import Routing.Duplex.Generic as G

data Route = Home | Activity | Signal SignalID

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ G.sum
  { "Home": G.noArgs
  , "Activity": path "activity" G.noArgs
  , "Signal": path "signal" (signalId segment)
  }

  where

    signalId :: RouteDuplex' String -> RouteDuplex' SignalID
    signalId = as SignalID.toString (SignalID.parse >>> note "Bad signalId")