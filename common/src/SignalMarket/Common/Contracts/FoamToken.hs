{-# LANGUAGE QuasiQuotes #-}

module SignalMarket.Common.Contracts.FoamToken where

import           Network.Ethereum.Contract.TH (abiFrom)


[abiFrom|../build/FoamToken.json|]
