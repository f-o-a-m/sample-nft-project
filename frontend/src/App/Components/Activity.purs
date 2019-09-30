module App.Components.Activity where

import Prelude

import App.API.Routes (getActivity, getSignals)
import App.Components.Common (SignalState, renderSignal)
import App.Data.Activity as Activity
import App.Data.Signal (Signal(..), signalId)
import App.Data.SignalId (SignalId)
import App.Data.User (User(..), isGuest)
import App.HTML (pushCanceler, whenHtml)
import Data.Array (filter)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Canceler(..), error, fiberCanceler, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Etherium.Tx as Tx
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as HH
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

type Props = { user :: User }
component :: React.Component Props
component = React.createComponent "Activity"

type State =
  { canceler :: Canceler
  , signals :: Maybe (Array (SignalState ()))
  , showOnlyByUser :: Boolean
  }
type Self = React.Self Props State

activity :: Props -> JSX
activity = React.make component
  { initialState:
      { canceler: mempty :: Canceler
      , activities: Nothing
      , showOnlyByUser: false
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount self = do
      launchAff_ $ un Canceler self.state.canceler $ error "willUnmount"
    
    didMount self = do
      signalsFiber <- launchAff $ getActivity >>= \activities -> do
        liftEffect $ self.setState _ {activities = Just $ activities}
      pushCanceler self $ fiberCanceler signalsFiber

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

      , case state.activities of
          Nothing -> HH.text "Loading ..."
          Just activities -> R.div_ $ (toFilter self activities) <#> renderActivity self
      ]
    renderActivity self activity' = R.div_ [R.text $ show activity']
    toFilter self@{props, state} activities = case props.user of
      UserGuest -> activities
      UserConnected {userAddress}
        | state.showOnlyByUser -> activities
          # filter (Array.elem userAddress <<< Activity.getUserAddresses)
        | otherwise -> activities

    
    overSignalSt :: Self -> SignalId -> (SignalState () -> SignalState ()) -> Effect Unit
    overSignalSt self signalId f = self.setState \s -> s
      { signals = s.signals <#> map \signalSt@{ signal: (Signal {id})} ->
          if signalId == id then f signalSt else signalSt
      }

