module Main where

import           SignalMarket.Server.Config

main :: IO ()
main = mkContracts >>= print

