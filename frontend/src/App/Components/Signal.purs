module App.Components.Signal where

import Prelude

import App.API (getSignal)
import App.Components.Avatar (avatar)
import App.Components.Common (renderSignal, renderToken)
import App.Data.Event (Event, eventToSignalId, eventToSignalActivity, eventToSignalUpdate)
import App.Data.Signal (Signal)
import App.Data.SignalActivity (SignalActivity(..))
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.SignalId (SignalId)
import App.Data.User (User)
import App.HTML (classy)
import App.HTML.Canceler (pushCanceler, runCancelers)
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (fiberCanceler, launchAff)
import Effect.Aff.Bus (BusR)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Etherium.Tx as Tx
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R

type Props =
  { events :: BusR Event
  , user :: User
  , sid :: SignalId
  }
component :: React.Component Props
component = React.createComponent "Signal"

type State = Maybe SignalState

type SignalState =
  { signal :: Signal
  , activity :: Array SignalActivity
  , tx :: Maybe Tx.Progress
  , price :: Maybe String
  }

type Self = React.Self Props State

signal :: Props -> JSX
signal = React.make component
  { initialState: Nothing
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount :: Self -> Effect Unit
    willUnmount self = runCancelers self

    didMount :: Self -> Effect Unit
    didMount self = do
      signalsFiber <- launchAff $ getSignal self.props.sid >>= \(SignalDetails {signal:s, activity}) -> do
        liftEffect $ self.setState \_ -> Just { tx: Nothing, price: Nothing, signal:s, activity }
      liveFib <- launchAff $ forever $ Bus.read self.props.events >>= \event -> liftEffect do
        when (eventToSignalId event == Just self.props.sid) do
          self.setState $ map \s -> s
            { activity = maybe [] pure (eventToSignalActivity event) <> s.activity
            , signal = fromMaybe s.signal $ eventToSignalUpdate event s.signal
            }

      pushCanceler self $ fiberCanceler signalsFiber <> fiberCanceler liveFib
      pure unit

    render :: Self -> JSX
    render self@{props, state} = React.fragment
      [ case state of
          Nothing -> R.text "Loading ..."
          Just signal' -> R.div_
            [ renderSignal
                { txSend: \a b c -> pushCanceler self =<< Tx.send a b c
                , updateState: \f -> self.setState (map f)
                , state: signal'
                , user: props.user
                , addLink: false
                , details: classy R.div "SignalActivity"
                    if Array.null signal'.activity
                      then
                        [ classy R.div "SignalActivity-header" [R.text "No Activity"] ]
                      else
                        [ classy R.div "SignalActivity-header" [R.text "Activity:"]
                        , classy R.ul "Activity" $ renderActivity <$> signal'.activity
                        ]
                }
            ]
      ]

    renderActivity = case _ of
      ListedForSale a -> R.li_
        [ R.text "Listed for sale by "
        , avatar a.owner
        , R.text " for "
        , renderToken a.price
        ]
      UnlistedFromSale a -> R.li_
        [ R.text "Unlisted from sale by "
        , avatar a.owner
        ]
      Sold a -> R.li_
        [ R.text "Sold to "
        , avatar a.buyer
        , R.text " by "
        , avatar a.owner
        , R.text " for "
        , renderToken a.price
        ]
