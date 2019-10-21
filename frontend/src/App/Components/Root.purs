module App.Components.Root where

import Prelude

import App.API (getContracts)
import App.API.WS as WS
import App.Components.Header (header)
import App.Components.Signal (signal)
import App.Components.Signals (signals)
import App.Data.Contracts (Contracts(..))
import App.Data.Event (Event)
import App.Data.ProviderState (ConnectedState)
import App.Data.ProviderState as ProviderState
import App.Data.User (User(..))
import App.Ethereum.Provider as EthProvider
import App.HTML (classy)
import App.HTML.Canceler (pushCanceler, pushCanceler', runCancelers, runCancelers')
import App.Route (Route)
import App.Route as Route
import Contracts.SignalMarket as SignalMarket
import Contracts.FoamToken as FoamToken
import Contracts.SignalToken as SignalToken
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (forever)
import Control.Parallel (parSequence)
import Data.Either (Either(..))
import Data.Foldable (fold, for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (Aff, Canceler, effectCanceler, fiberCanceler, joinFiber, launchAff, launchAff_, never)
import Effect.Aff.BRef as BRef
import Effect.Aff.Bus (BusR, BusW)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (eventFilter)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import Routing.Duplex as RD
import Routing.Duplex.Parser (RouteError)
import Routing.Hash (getHash, setHash)
import Routing.Hash as RoutingHash
import Type.Proxy (Proxy(..))
import Data.Symbol (SProxy(..))

type Props =
  { events :: Tuple (BusR Event) (BusW Event)
  }
component :: React.Component Props
component = React.createComponent "Root"

type State =
  { route :: Either RouteError Route
  , providerState :: ProviderState.State
  }
type Self = React.Self Props State

data Action
  = SomeAction
  | NewRoute (Either RouteError Route)

root :: Props -> JSX
root = React.make component
  { initialState:
      { route: Right Route.Signals
      , providerState: ProviderState.Unknown
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount :: Self -> Effect Unit
    willUnmount self = runCancelers self
    didMount :: Self -> Effect Unit
    didMount self = do
      -- redirect to `/#/` if user is visiting `/`
      whenM (String.null <$> getHash ) $ setHash $ Route.href Route.Signals

      contractsFiber <- launchAff $ getContracts
      pushCanceler self $ fiberCanceler contractsFiber

      providerMb <- runMaybeT
        $ map Left (MaybeT EthProvider.getEthereumProvider)
        <|> map Right (MaybeT EthProvider.getLegacyProvider)

      let
        setProviderState ps = do
          self.setState _ { providerState = ps}
          runCancelers' "openFilters" self
          for_ (ProviderState.viewConnectedState ps) \con -> do
            fib <- launchAff do
              canceler <- openFilters (snd self.props.events) con
              liftEffect $ pushCanceler' "openFilters" self canceler
            pushCanceler' "openFilters" self $ fiberCanceler fib

        startConnectivityLoop provider contracts@(Contracts {networkId}) = do
          Tuple connectivityBRef connectivityFiber <- EthProvider.live
            { expectedNetwork: networkId
            , reconnectionDelay: Milliseconds 1000.0
            , provider
            }
          pushCanceler self $ fiberCanceler connectivityFiber
          let
            storeConnectivityChange = traverse_ \s ->
              setProviderState $ ProviderState.Enabled
                { connectivity: s, provider: EthProvider.toWeb3Provider provider, contracts }
          BRef.read connectivityBRef >>= storeConnectivityChange
          providerUpdateFiber <- launchAff $ forever $ BRef.readOnUpdate connectivityBRef
            >>= storeConnectivityChange
            >>> liftEffect
          pushCanceler self $ fiberCanceler providerUpdateFiber

      case providerMb of
        Nothing -> do
          setProviderState $ ProviderState.NotInjected
        Just (Left ethereumProvider) -> do
          setProviderState $ ProviderState.Injected {loading: false}
          fib <- launchAff $ EthProvider.enable ethereumProvider >>= case _ of
            Nothing -> do
              liftEffect $ setProviderState $ ProviderState.Rejected ethereumProvider
              never
            Just ethereumProviderEnabled -> do
              liftEffect $ setProviderState $ ProviderState.Injected {loading: true}
              joinFiber contractsFiber
                >>= (startConnectivityLoop ethereumProviderEnabled >>> liftEffect)
          pushCanceler self $ fiberCanceler fib
        Just (Right legacyProvider) -> do
          setProviderState $ ProviderState.Injected {loading: true}
          fib <- launchAff $ joinFiber contractsFiber
            >>= (startConnectivityLoop legacyProvider >>> liftEffect)
          pushCanceler self $ fiberCanceler fib

      routingCanceller <- RoutingHash.matchesWith (RD.parse Route.routeCodec >>> Just) \old new -> do
        self.setState _ { route = new }
      pushCanceler self $ effectCanceler routingCanceller

    render :: Self -> JSX
    render self@{state} = R.div_
      [ header { providerState: state.providerState }
      , navigation state.route
      , classy R.div "Content" $ pure case state.route of
          Left err -> classy R.h1 "Content-404" [ R.text "404" ]
          Right (Route.Signals) -> signals
            { events: fst self.props.events
            , user
            }
          Right (Route.Signal sid) -> React.keyed (show sid) $ signal
            { events: fst self.props.events
            , user
            , sid
            }
      ]
      where
        user = maybe UserGuest UserConnected $ ProviderState.viewConnectedState state.providerState

    navigation activeRoute = classy R.div "Navigation" $ pure $
      if Right Route.Signals == activeRoute
        then R.span
          { children: [ R.text "Home" ]
          , className: "Navigation-item Navigation-item--active"
          }
        else R.a
          { href: Route.href Route.Signals
          , children: [ R.text $ "Home" ]
          , className: "Navigation-item"
          }


openFilters :: BusW Event -> ConnectedState -> Aff Canceler
openFilters bus {provider, contracts: (Contracts c)} = do
  ws <- WS.open
  let emit = \e -> launchAff_ $ Bus.write e bus
  fold <$> parSequence
    [ WS.event ws
        (eventFilter (Proxy :: Proxy FoamToken.Transfer) c.foamToken)
        (V.inj (SProxy :: SProxy "transfer") >>> emit)
    , WS.event ws
        (eventFilter (Proxy :: Proxy SignalMarket.SignalForSale) c.signalMarket)
        (V.inj (SProxy :: SProxy "signalForSale") >>> emit)
    , WS.event ws
        (eventFilter (Proxy :: Proxy SignalMarket.SignalUnlisted) c.signalMarket)
        (V.inj (SProxy :: SProxy "signalUnlisted") >>> emit)
    , WS.event ws
        (eventFilter (Proxy :: Proxy SignalMarket.SignalSold) c.signalMarket)
        (V.inj (SProxy :: SProxy "signalSold") >>> emit)
    , WS.event ws
        (eventFilter (Proxy :: Proxy SignalToken.TrackedToken) c.signalToken)
        (V.inj (SProxy :: SProxy "trackedToken") >>> emit)
    ]
