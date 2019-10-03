module App.Components.Signals where

import Prelude

import App.API (getSignals)
import App.Components.Common (SignalState, renderLinkedCollection, renderSignal)
import App.Data.Collections (LinkedCollection, initialCursor, initialLinkedCollection, insertCollection)
import App.Data.Signal (Signal(..), signalId)
import App.Data.SignalId (SignalId)
import App.Data.User (User(..), isGuest)
import App.HTML (pushCanceler, whenHtml)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Canceler(..), error, fiberCanceler, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Etherium.Tx as Tx
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

type Props = { user :: User }
component :: React.Component Props
component = React.createComponent "Signals"

type State =
  { canceler :: Canceler
  , signals :: LinkedCollection (SignalState ())
  , showOnlyByUser :: Boolean
  }
type Self = React.Self Props State

signals :: Props -> JSX
signals = React.make component
  { initialState:
      { canceler: mempty :: Canceler
      , signals: initialLinkedCollection
      , showOnlyByUser: false
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount self = do
      launchAff_ $ un Canceler self.state.canceler $ error "willUnmount"
    load self cursor = do
      liftEffect $ self.setState _ {signals{loading = true}}
      signalsFiber <- launchAff $ getSignals cursor >>= \{items, next} -> do
        liftEffect $ self.setState \s -> s {signals = s.signals `insertCollection` {next, items: items <#> ({ tx: Nothing, price: Nothing, signal:_ })}}
      pushCanceler self $ fiberCanceler signalsFiber

    didMount self = do
      load self initialCursor

    render self@{props, state} = React.fragment
      [ whenHtml (not isGuest props.user) \_ -> R.button
          { onClick: capture_ do
              self.setState _ {showOnlyByUser = not state.showOnlyByUser}
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

