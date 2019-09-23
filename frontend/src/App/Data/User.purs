module App.Data.User
  ( User(..)
  ) where

import App.Ethereum.Provider (ConnectedState)

data User
  = UserGuest
  | UserConnected ConnectedState