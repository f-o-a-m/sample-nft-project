module App.Components.Root where

import Prelude

import App.Components.Header (header)
import App.Data.User (User(..))
import App.Ethereum.Provider (State(..), getEnabledEthereumProviderState, viewConnectedState)
import App.Route (Route)
import App.Route as Route
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (un)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (traceM)
import Effect.Aff (Canceler(..), effectCanceler, error, fiberCanceler, launchAff, launchAff_)
import Effect.Aff.BRef as BRef
import Effect.Class (liftEffect)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import Routing.Duplex as RD
import Routing.Duplex.Parser (RouteError)
import Routing.Hash (getHash, setHash)
import Routing.Hash as RoutingHash

component :: React.Component Unit
component = React.createComponent "Root"
  
data Action 
  = SomeAction
  | NewRoute (Either RouteError Route)

root :: JSX
root = unit # React.make component
  { initialState:
      { route: Right Route.Home
      , providerState: NotInjected
      , canceler: mempty :: Canceler
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount self = do
      launchAff_ $ un Canceler self.state.canceler $ error "willUnmount"
    didMount self = do
      -- redirect to `/#/` if user is visiting `/`
      whenM (String.null <$> getHash ) $ setHash "/"

      providerBRef <- getEnabledEthereumProviderState
        { expectedNetwork: "420123"
        , reconnectionDelay: Milliseconds 1000.0
        }

      providerUpdateFiber <- launchAff $ forever $ BRef.readOnUpdate providerBRef >>= \s ->
        liftEffect $ self.setState _ { providerState = s }

      routingCanceller <- RoutingHash.matchesWith (RD.parse Route.routeCodec >>> Just) \old new -> do
        traceM $ show {old, new}
        self.setState _ { route = new }

      providerState <- BRef.read providerBRef

      self.setState _
        { providerState = providerState
        , canceler = fiberCanceler providerUpdateFiber <> effectCanceler routingCanceller
        }

    render self@{state} = R.div_
      [ header { providerState: state.providerState }
      , case state.route of
          Left err -> R.h1_ [ R.text "404" ]
          Right (Route.Home) -> R.h1_ [ R.text "Home" ]
          Right (Route.Activity) -> R.h1_ [ R.text "Activity" ]
          Right (Route.Signal sid) -> R.h1_ [ R.text "Signal" ]
      ]
      where
        -- TODO pass `user` to children components
        user = maybe UserGuest UserConnected $ viewConnectedState state.providerState