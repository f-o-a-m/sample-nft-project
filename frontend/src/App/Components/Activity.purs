module App.Components.Activity where

import Prelude

import App.API (getActivity)
import App.Components.Common (SignalState, renderLinkedCollection)
import App.Data.Activity as Activity
import App.Data.Collections (LinkedCollection, initialCursor, initialLinkedCollection, insertCollection)
import App.Data.User (User(..), isGuest)
import App.HTML (pushCanceler, whenHtml)
import Data.Array (filter)
import Data.Array as Array
import Data.Newtype (un)
import Effect.Aff (Canceler(..), error, fiberCanceler, launchAff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

type Props = { user :: User }
component :: React.Component Props
component = React.createComponent "Activity"

type State =
  { canceler :: Canceler
  , signals :: LinkedCollection (SignalState ())
  , showOnlyByUser :: Boolean
  }
type Self = React.Self Props State

activity :: Props -> JSX
activity = React.make component
  { initialState:
      { canceler: mempty :: Canceler
      , activities: initialLinkedCollection
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
      liftEffect $ self.setState _ {activities{loading = true}}
      signalsFiber <- launchAff $ getActivity cursor >>= \collection -> do
        liftEffect $ self.setState \s -> s {activities = s.activities `insertCollection` collection}
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
          state.activities
          (load self)
          (\items -> R.div_ $ (renderActivity self) <$> toFilter self items)
      ]
    renderActivity self activity' = R.div_ [R.text $ show activity']
    toFilter self@{props, state} activities = case props.user of
      UserGuest -> activities
      UserConnected {userAddress}
        | state.showOnlyByUser -> activities
          # filter (Array.elem userAddress <<< Activity.getUserAddresses)
        | otherwise -> activities
