module Main where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Chanterelle.Internal.Logging (log, LogLevel(Info))
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Either (fromRight)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Web3 (Address, ChainCursor(Latest), _from, _gas, _to, defaultTransactionOptions, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal, embed)
import Partial.Unsafe (unsafePartial)
import Contracts.SimpleStorage as SS

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
