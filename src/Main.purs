module Main where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (_from, _gas, defaultTransactionOptions)

import Partial.Unsafe (unsafePartial)

-- `chanterelle` - required for deployment
simpleStorageConfig :: ContractConfig NoArgs
simpleStorageConfig =
  { -- solc build artifact (chanterelle compiled)
    filepath : "./build/SimpleStorage.json"
  , name : "SimpleStorage"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }

type DeployResults =
  { simpleStorage :: DeployReceipt NoArgs
  }

-- web3 connection
-- npm run build this
-- then chanterelle deploy ./output/blah/index.js
deployScript :: DeployM DeployResults
deployScript = do
  deployCfg@(DeployConfig {primaryAccount, provider}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  simpleStorage <- deployContract txOpts simpleStorageConfig
  pure { simpleStorage }
