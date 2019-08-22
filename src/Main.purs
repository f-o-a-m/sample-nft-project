module Main where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (Address, _from, _gas, defaultTransactionOptions)
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken

import Partial.Unsafe (unsafePartial)

deploy :: DeployM Unit
deploy = void deployScript

-- `chanterelle` - required for deployment
simpleStorageConfig :: ContractConfig NoArgs
simpleStorageConfig =
  { -- solc build artifact (chanterelle compiled)
    filepath: "./build/SimpleStorage.json"
  , name: "SimpleStorage"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

-- foamtoken also has no args
foamTokenConfig :: ContractConfig NoArgs
foamTokenConfig =
  {
    filepath: "./build/FoamToken.json"
  , name: "FoamToken"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }

-- when constructor has arguments
type SignalTokenConstructorArgs = (_token :: Address)

makeSignalTokenConfig :: Record SignalTokenConstructorArgs ->
                         ContractConfig SignalTokenConstructorArgs
makeSignalTokenConfig { _token } =
  {
    filepath: "./build/SignalToken.json"
  , name: "SignalToken"
  , constructor: SignalToken.constructor
  , unvalidatedArgs: pure { _token }
  }

type SignalMarketConstructorArgs = (_signalToken :: Address, _foamToken :: Address)

makeSignalMarketConfig :: Record SignalMarketConstructorArgs ->
                          ContractConfig SignalMarketConstructorArgs
makeSignalMarketConfig { _signalToken, _foamToken } =
  {
    filepath: "./build/SignalMarket.json"
  , name: "SignalMarket"
  -- from chanterelle-generated PS files
  , constructor: SignalMarket.constructor
  -- required to be a record for the V monad
  , unvalidatedArgs: pure { _signalToken, _foamToken }
  }

type DeployResults =
  ( simpleStorage :: DeployReceipt NoArgs
  , foamToken :: DeployReceipt NoArgs
  )

-- web3 connection
-- npm run build this
-- then chanterelle deploy ./output/blah/index.js
deployScript :: DeployM (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount, provider}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  simpleStorage <- deployContract txOpts simpleStorageConfig
  foamToken <- deployContract txOpts foamTokenConfig
  pure { simpleStorage, foamToken }
