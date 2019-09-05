module SignalMarket.Server.Config
    ( Contracts(..)
    ) where

import Data.Solidity.Prim.Address (Address)

data Contracts = Contracts
  { contractsFoamToken :: Address 
  , contractsSignalToken :: Address
  , contractsSignalMarket :: Address
  }