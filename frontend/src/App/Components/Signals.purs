module App.Components.Signals where

import Prelude

import App.API (getSignalTokenWithSales)
import App.Components.Common (SignalState, renderSignal)
import App.Data.Collections (LinkedCollection, Cursor, initialCursor, initialLinkedCollection, insertCollection)
import App.Data.Event (Event, eventToSignal, eventToSignalUpdate)
import App.Data.Signal (Signal(..), signalId)
import App.Data.SignalId (SignalId)
import App.Data.User (User(..), isGuest)
import App.HTML (classy, maybeHtml, whenHtml)
import App.HTML.Canceler (pushCanceler, runCancelers)
import Control.Monad.Rec.Class (forever)
import Control.MonadZero as MZ
import Data.Array (filter)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Effect (Effect)
import Effect.Aff (fiberCanceler, launchAff)
import Effect.Aff.Bus (BusR)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Etherium.Tx as Tx
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

type Props =
  { events :: BusR Event
  , user :: User
  }
component :: React.Component Props
component = React.createComponent "Signals"

type State =
  { signals :: LinkedCollection (SignalState ())
  , showOnlyByUser :: Boolean
  }

type Self = React.Self Props State

signals :: Props -> JSX
signals = React.make component
  { initialState:
      { signals: initialLinkedCollection
      , showOnlyByUser: false
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount :: Self -> Effect Unit
    willUnmount self = runCancelers self

    injSignal = { tx: Nothing, price: Nothing, signal:_ }

    load self cursor = do
      liftEffect $ self.setState _ {signals{loading = true}}
      signalsFiber <- launchAff $ getSignalTokenWithSales cursor >>= \{items, next} -> do
        liftEffect $ self.setState \s -> s {signals = s.signals `insertCollection` {next, items: items <#> injSignal}}
      pushCanceler self $ fiberCanceler signalsFiber

    didMount :: Self -> Effect Unit
    didMount self = do
      load self initialCursor
      liveFib <- launchAff $ forever $ Bus.read self.props.events >>= \event -> liftEffect $ self.setState \state ->
        let newSignal = eventToSignal event
        in state
          { signals
              { items = maybe [] (pure <<< injSignal) newSignal <> state.signals.items <#> \signalState ->
                let update = eventToSignalUpdate event signalState.signal
                in signalState
                  { signal = fromMaybe signalState.signal update
                  , tx = if isJust update then Nothing else signalState.tx
                  }
              , next = state.signals.next <#> \n -> n{offset = maybe 0 (const 1) newSignal + n.offset}
              }
          }
      pushCanceler self $ fiberCanceler liveFib

    render :: Self -> JSX
    render self@{props, state} = React.fragment
      [ whenHtml (not isGuest props.user) \_ -> R.button
          { onClick: capture_ do
              self.setState _ {showOnlyByUser = not state.showOnlyByUser}
          , className: "Content-control"
          , children:
              [ R.text if state.showOnlyByUser
                  then "Show All"
                  else "Show My"
              ]
          }
      , renderLinkedCollection
          state.signals
          (load self)
          (\items -> R.div_ $ toFilter self items <#> \s -> renderSignal
            { txSend: \a b c -> pushCanceler self =<< Tx.send a b c
            , updateState: overSignalSt self $ signalId s.signal
            , state: s
            , user: props.user
            , details: mempty
            , addLink: true
            })
      ]

    toFilter self@{props, state} signals' = case props.user of
      UserGuest -> signals'
      UserConnected {userAddress}
        | state.showOnlyByUser -> filter (\{signal: (Signal {owner})} -> owner == userAddress) signals'
        | otherwise -> signals'


    overSignalSt :: Self -> SignalId -> (SignalState () -> SignalState ()) -> Effect Unit
    overSignalSt self signalId f = self.setState \s -> s
      { signals{ items= s.signals.items <#> \signalSt@{ signal: (Signal {id})} ->
          if signalId == id then f signalSt else signalSt
      }}

    renderLinkedCollection
      :: forall a
       . LinkedCollection a
      -> (Cursor -> Effect Unit)
      -> (Array a -> JSX)
      -> JSX
    renderLinkedCollection {items, next, loading} loadMore renderItems = React.fragment
      [ renderItems items
      , classy R.div "LoadingMore"
          [ maybeHtml (MZ.guard (not loading) *> next) \cursor -> R.button
              { onClick: capture_ $ loadMore cursor
              , children: [ R.text "Load more" ]
              }
          , whenHtml loading \_ -> R.text "Loading ..."
          ]
      ]
