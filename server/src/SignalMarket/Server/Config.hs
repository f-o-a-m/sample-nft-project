module SignalMarket.Server.Config
    ( Contracts(..)
    , mkContracts
    ) where

import           Data.Solidity.Prim.Address       (Address)
import           SignalMarket.Server.Config.Utils

mkContracts :: IO Contracts
mkContracts = makeConfig $ do
  networkID <- getEnvVar "NETWORK_ID"
  foamTokenAddress <- getContractAddress "FoamToken" networkID
  signalTokenAddress <- getContractAddress "SignalToken" networkID
  signalMarketAddress <- getContractAddress "SignalMarket" networkID
  return $ Contracts
    { contractsFoamToken = foamTokenAddress
    , contractsSignalToken = signalTokenAddress
    , contractsSignalMarket = signalMarketAddress
    }

data Contracts = Contracts
  { contractsFoamToken    :: Address
  , contractsSignalToken  :: Address
  , contractsSignalMarket :: Address
  } deriving (Eq, Show)
