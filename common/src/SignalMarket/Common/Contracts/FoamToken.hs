{-# LANGUAGE QuasiQuotes #-}

module SignalMarket.Common.Contracts.FoamToken where

import           Network.Ethereum.Contract.TH     (abiFrom)
import           SignalMarket.Common.Config.Types (HasEventName (..))


[abiFrom|../build/FoamToken.json|]

instance HasEventName Transfer where
  eventName _ = "FoamTokenTransfer"
