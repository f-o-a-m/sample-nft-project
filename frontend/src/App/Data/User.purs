module App.Data.User where

import App.Data.ProviderState (ConnectedState)

data User
  = UserGuest
  | UserConnected ConnectedState

isGuest :: User -> Boolean
isGuest UserGuest = true
isGuest (UserConnected _) = false
