module App.Components.Root where

import Prelude

import App.API.Routes (getContracts)
import App.Components.Header (header)
import App.Data.Contracts (Contracts(..))
import App.Data.ProviderState as ProviderState
-- (State(..), viewConnectedState)
import App.Data.User (User(..))
import App.Ethereum.Provider as EthProvider
import App.Route (Route)
import App.Route as Route
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Canceler(..), effectCanceler, error, fiberCanceler, joinFiber, launchAff, launchAff_, never)
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
      , providerState: ProviderState.Unknown
      , canceler: mempty :: Canceler
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount self = do
      launchAff_ $ un Canceler self.state.canceler $ error "willUnmount"

    pushCanceler self c = self.setState \s -> s {canceler = s.canceler <> c}

    didMount self = do
      -- redirect to `/#/` if user is visiting `/`
      whenM (String.null <$> getHash ) $ setHash "/"

      contractsFiber <- launchAff $ getContracts
      pushCanceler self $ fiberCanceler contractsFiber

      providerMb <- runMaybeT
        $ map Left (MaybeT EthProvider.getEthereumProvider)
        <|> map Right (MaybeT EthProvider.getLegacyProvider)

      let
        startConnectivityLoop provider contracts@(Contracts {networkId}) = do
          Tuple connectivityBRef connectivityFiber <- EthProvider.live
            { expectedNetwork: networkId
            , reconnectionDelay: Milliseconds 1000.0
            , provider
            }
          pushCanceler self $ fiberCanceler connectivityFiber
          initialConnectivityState <- BRef.read connectivityBRef
          liftEffect $ self.setState _
            { providerState = ProviderState.Enabled initialConnectivityState (EthProvider.toWeb3Provider provider) contracts }
          providerUpdateFiber <- launchAff $ forever $ BRef.readOnUpdate connectivityBRef >>= \s ->
            liftEffect $ self.setState _
              { providerState = ProviderState.Enabled s (EthProvider.toWeb3Provider provider) contracts }
          pushCanceler self $ fiberCanceler providerUpdateFiber

      case providerMb of
        Nothing -> do
          self.setState _ {providerState = ProviderState.NotInjected}
        Just (Left ethereumProvider) -> do
          self.setState _ {providerState = ProviderState.Injected}
          fib <- launchAff $ EthProvider.enable ethereumProvider >>= case _ of
            Nothing -> do
              liftEffect $ self.setState _ {providerState = ProviderState.Rejected ethereumProvider}
              never
            Just ethereumProviderEnabled -> do
              joinFiber contractsFiber 
                >>= (startConnectivityLoop ethereumProviderEnabled >>> liftEffect)
          pushCanceler self $ fiberCanceler fib
        Just (Right legacyProvider) -> do
          self.setState _ {providerState = ProviderState.Injected}
          fib <- launchAff $ joinFiber contractsFiber 
            >>= (startConnectivityLoop legacyProvider >>> liftEffect)
          pushCanceler self $ fiberCanceler fib

      routingCanceller <- RoutingHash.matchesWith (RD.parse Route.routeCodec >>> Just) \old new -> do
        self.setState _ { route = new }
      pushCanceler self $ effectCanceler routingCanceller

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
        user = maybe UserGuest UserConnected $ ProviderState.viewConnectedState state.providerState