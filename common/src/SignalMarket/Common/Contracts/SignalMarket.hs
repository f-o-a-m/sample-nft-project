{-# LANGUAGE QuasiQuotes #-}

module SignalMarket.Common.Contracts.SignalMarket where

import           Network.Ethereum.Contract.TH     (abiFrom)
import           SignalMarket.Common.Config.Types (HasEventName (..))

[abiFrom|../build/SignalMarket.json|]

instance HasEventName SignalForSale where
  eventName _ = "SignalMarketSignalForSale"

instance HasEventName SignalSold where
  eventName _ = "SignalMarketSignalSold"
