module Main where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (Address, _from, _gas, defaultTransactionOptions)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Partial.Unsafe (unsafePartial)

deploy :: DeployM Unit
deploy = void deployScript

-- `chanterelle` - required for deployment
simpleStorageConfig :: ContractConfig NoArgs
simpleStorageConfig =
  { -- solc build artifact (chanterelle compiled)
    filepath : "./build/contracts/SimpleStorage.json"
  , name : "SimpleStorage"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }

type DeployResults = ( simpleStorage :: Address )

-- web3 connection
deployScript :: DeployM (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  simpleStorage <- deployContract txOpts simpleStorageConfig
  pure { simpleStorage: simpleStorage.deployAddress }
