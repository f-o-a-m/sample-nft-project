{-# LANGUAGE QuasiQuotes #-}

module SignalMarket.Common.Contracts.SignalToken where

import           Network.Ethereum.Contract.TH (abiFrom)

[abiFrom|../build/SignalToken.json|]
