module App.Components.Activity (activity, Props) where

import Prelude

import App.API (getActivity, getSignal)
import App.Components.Avatar (avatar)
import App.Components.Common (renderGeoHash, renderLinkedCollection, renderToken)
import App.Data.Activity (Activity(..))
import App.Data.Activity as Activity
import App.Data.Collections (initialCursor, initialLinkedCollection, insertCollection)
import App.Data.Event (Event, eventToActivity)
import App.Data.Signal (Signal(..))
import App.Data.SignalDetails (SignalDetails(..))
import App.Data.User (User(..), isGuest)
import App.HTML (classy, whenHtml)
import App.HTML.Canceler (pushCanceler, runCancelers)
import App.Route as Route
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Array (filter)
import Data.Array as Array
import Data.Maybe (maybe)
import Data.Newtype (un)
import Effect.Aff (fiberCanceler, launchAff)
import Effect.Aff.Bus (BusR)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

type Props =
  { events :: BusR Event
  , user :: User
  }

component :: React.Component Props
component = React.createComponent "Activity"

activity :: Props -> JSX
activity = React.make component
  { initialState:
      { activities: initialLinkedCollection
      , showOnlyByUser: false
      }
  , render
  , didMount
  , willUnmount
  }
  where
    willUnmount self = runCancelers self
    didMount self = do
      load self initialCursor
      liveFib <- launchAff $ forever $ Bus.read self.props.events >>= \event -> liftEffect do
        fib <- launchAff do
          act <- runReaderT (eventToActivity event) (getSignal >>> map (un SignalDetails >>> _.signal))
          liftEffect $ self.setState \state -> state {activities{items = maybe [] pure act <> state.activities.items}}
        pushCanceler self $ fiberCanceler fib

      pushCanceler self $ fiberCanceler liveFib

    load self cursor = do
      liftEffect $ self.setState _ {activities{loading = true}}
      signalsFiber <- launchAff $ getActivity cursor >>= \collection -> do
        liftEffect $ self.setState \s -> s {activities = s.activities `insertCollection` collection}
      pushCanceler self $ fiberCanceler signalsFiber

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
          state.activities
          (load self)
          (\items -> classy R.ul "Activity" $ renderActivity <$> toFilter self items)
      ]


    toFilter self@{props, state} activities = case props.user of
      UserGuest -> activities
      UserConnected {userAddress}
        | state.showOnlyByUser -> activities
          # filter (Array.elem userAddress <<< Activity.getUserAddresses)
        | otherwise -> activities

    renderActivity = case _ of
      TokenTransfer a -> R.li_
        [ R.text "Transfer of "
        , renderToken a.amount
        , R.text " from "
        , avatar a.from
        , R.text " to "
        , avatar a.to
        ]
      SignalListedForSale a -> R.li_
        [ R.a
            { href: Route.href $ Route.Signal (un Signal a.signal).id
            , children: [ R.text $ "Signal " <> renderGeoHash (un Signal a.signal).geohash]
            }
        , R.text " with stake "
        , renderToken (un Signal a.signal).stake
        , R.text " was listed for sale by "
        , avatar a.owner
        , R.text " for "
        , renderToken a.price
        ]
      SignalUnlistedFromSale a -> R.li_
        [ R.a
            { href: Route.href $ Route.Signal (un Signal a.signal).id
            , children: [ R.text $ "Signal " <> renderGeoHash (un Signal a.signal).geohash]
            }
        , R.text " with stake "
        , renderToken (un Signal a.signal).stake
        , R.text " was unlisted from sale by "
        , avatar a.owner
        ]
      SignalSold a -> R.li_
        [ R.a
            { href: Route.href $ Route.Signal (un Signal a.signal).id
            , children: [ R.text $ "Signal " <> renderGeoHash (un Signal a.signal).geohash]
            }
        , R.text " with stake "
        , renderToken (un Signal a.signal).stake
        , R.text " was sold to "
        , avatar a.buyer
        , R.text " by "
        , avatar a.owner
        , R.text " for "
        , renderToken a.price
        ]
