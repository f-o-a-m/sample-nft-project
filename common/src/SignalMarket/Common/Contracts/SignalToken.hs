{-# LANGUAGE QuasiQuotes #-}

module SignalMarket.Common.Contracts.SignalToken where

import           Network.Ethereum.Contract.TH     (abiFrom)
import           SignalMarket.Common.Config.Types (HasEventName (..))

[abiFrom|../build/SignalToken.json|]

instance HasEventName Transfer where
  eventName _ = "SignalTokenTransfer"

instance HasEventName TrackedToken where
  eventName _ = "SignalTokenTrackedToken"
