module App.Components.Signal where

import Prelude

import App.API (getSignal)
import App.Components.Common (renderSignal)
import App.Data.Signal (Signal)
import App.Data.SignalActivity (SignalActivity)
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.SignalId (SignalId)
import App.Data.User (User)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Canceler(..), error, fiberCanceler, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Etherium.Tx as Tx
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R

type Props = { user :: User, sid :: SignalId }
component :: React.Component Props
component = React.createComponent "Signal"

type State =
  { canceler :: Canceler
  , signal :: Maybe SignalState
  }

type SignalState =
  { signal :: Signal
  , activity :: Array SignalActivity
  , tx :: Maybe Tx.Status
  , price :: Maybe String
  }

type Self = React.Self Props State

signal :: Props -> JSX
signal = React.make component
  { initialState:
      { canceler: mempty :: Canceler
      , signal: Nothing
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount self = do
      launchAff_ $ un Canceler self.state.canceler $ error "willUnmount"

    didMount :: Self -> Effect Unit
    didMount self = do
      signalsFiber <- launchAff $ getSignal self.props.sid >>= \(SignalDetails {signal:s, activity}) -> do
        liftEffect $ self.setState _
          {signal = Just { tx: Nothing, price: Nothing, signal:s, activity }}
      pushCanceler self $ fiberCanceler signalsFiber
      pure unit

    pushCanceler self c = self.setState \s -> s {canceler = s.canceler <> c}

    render :: Self -> JSX
    render self@{props, state} = React.fragment
      [ case state.signal of
          Nothing -> R.text "Loading ..."
          Just signal' -> R.div_
            [ renderSignal
                { txSend: \a b c -> pushCanceler self =<< Tx.send a b c
                , updateState: \f -> self.setState \s -> s { signal = f <$> s.signal }
                , state: signal'
                , user: props.user
                }
            , R.div_ [R.text "activity"]
            , R.div_ $ renderActivity <$> signal'.activity
            ]
      ]

    renderActivity a = R.div_ [R.text $ show a]

