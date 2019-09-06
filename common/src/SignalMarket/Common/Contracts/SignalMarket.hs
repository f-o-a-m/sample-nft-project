{-# LANGUAGE QuasiQuotes #-}

module SignalMarket.Common.Contracts.SignalMarket where

import           Network.Ethereum.Contract.TH (abiFrom)

[abiFrom|../build/SignalMarket.json|]
