module App.Data.User
  ( User(..)
  ) where

import App.Data.ProviderState (ConnectedState)

data User
  = UserGuest
  | UserConnected ConnectedState