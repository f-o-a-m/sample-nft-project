module App.Components.Signal where

import Prelude

import App.API (getSignal)
import App.Components.Avatar (avatar)
import App.Components.Common (renderDateTime, renderSignal, renderToken)
import App.Data.Signal (Signal)
import App.Data.SignalActivity (SignalActivity(..))
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.SignalId (SignalId)
import App.Data.User (User)
import App.HTML (classy)
import App.HTML.Canceler (pushCanceler, runCancelers)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (fiberCanceler, launchAff)
import Effect.Class (liftEffect)
import Etherium.Tx as Tx
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R

type Props = { user :: User, sid :: SignalId }
component :: React.Component Props
component = React.createComponent "Signal"

type State =
  { signal :: Maybe SignalState
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
      { signal: Nothing
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount self = runCancelers self

    didMount :: Self -> Effect Unit
    didMount self = do
      signalsFiber <- launchAff $ getSignal self.props.sid >>= \(SignalDetails {signal:s, activity}) -> do
        liftEffect $ self.setState _
          {signal = Just { tx: Nothing, price: Nothing, signal:s, activity }}
      pushCanceler self $ fiberCanceler signalsFiber
      pure unit

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
                , addLink: false
                , details: classy R.div "SignalActivity"
                    [ R.div_ [R.text "Activity:"]
                    , classy R.ul "Activity" $ renderActivity <$> signal'.activity
                    ]
                }

            ]
      ]

    renderActivity = case _ of
      ListedForSale a -> R.li_
        [ R.text "Listed for sale by "
        , avatar a.owner
        , R.text " at "
        , R.text $ renderDateTime a.timestamp
        , R.text " for "
        , renderToken a.price
        ]
      Soled a -> R.li_
        [ R.text "Soled to "
        , avatar a.buyer
        , R.text " by "
        , avatar a.owner
        , R.text " at "
        , R.text $ renderDateTime a.timestamp
        , R.text " for "
        , renderToken a.price
        ]
