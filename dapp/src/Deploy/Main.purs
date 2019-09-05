module Deploy.Main where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs, throwDeploy)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.Lens ((?~), (^?))
import Data.Maybe (fromJust)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (Address, _from, _gas, _to, defaultTransactionOptions, runWeb3)
import Contracts.SignalMarket as SignalMarket
import Deploy.Utils (awaitTxSuccess)
import Partial.Unsafe (unsafePartial)
import Contracts.FoamToken as FoamToken
import Contracts.SignalToken as SignalToken

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

tokenControllerMockConfig :: ContractConfig NoArgs
tokenControllerMockConfig =
  { filepath: "./build/TokenControllerMock.json"
  , name: "TokenControllerMock"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs

  }

-- when constructor has arguments
type SignalToken = (_token :: Address)

makeSignalTokenConfig :: Record SignalToken -> ContractConfig SignalToken
makeSignalTokenConfig { _token } =
  {
    filepath: "./build/SignalToken.json"
  , name: "SignalToken"
  , constructor: SignalToken.constructor
  , unvalidatedArgs: pure { _token }
  }

type SignalMarket = (_signalToken :: Address, _foamToken :: Address)

makeSignalMarketConfig :: Record SignalMarket -> ContractConfig SignalMarket
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
  , signalToken :: DeployReceipt SignalToken
  , signalMarket :: DeployReceipt SignalMarket
  , tokenFaucet :: Address
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
  -- deploy simple storage
  simpleStorage <- deployContract txOpts simpleStorageConfig
  -- deploy the mock token controller
  controller <- deployContract txOpts tokenControllerMockConfig
  -- deploy the FoamToken contract and set the controller
  foamToken <- deployContract txOpts foamTokenConfig
  let setControllerFTOpts = txOpts # _to ?~ foamToken.deployAddress
  setControllerFn FoamToken.setController provider setControllerFTOpts controller.deployAddress
  -- deploy the SignalToken contract and set the controller
  let signalTokenCfg = makeSignalTokenConfig { _token: foamToken.deployAddress }
  signalToken <- deployContract txOpts signalTokenCfg
  let setControllerSignalOpts = txOpts # _to ?~ signalToken.deployAddress
  setControllerFn SignalToken.setController provider setControllerSignalOpts controller.deployAddress
  -- deploy the SignalMarket contract
  signalMarket <- deployContract txOpts $
                  makeSignalMarketConfig { _signalToken: signalToken.deployAddress
                                         , _foamToken: foamToken.deployAddress
                                         }
  pure { simpleStorage, foamToken, signalToken, signalMarket, tokenFaucet: primaryAccount }
    where
      assertControllerSet provider addr action = do
        eRes <- liftAff $ runWeb3 provider action
        case eRes of
          Left _ -> throwDeploy $ error $ "Unable to set token controller for contract " <> show addr
          Right _ -> pure unit
      setControllerFn setTokenFn provider opts _controller = assertControllerSet provider (opts ^? _to) do
        txHash <- setTokenFn opts {_controller}
        liftAff $ awaitTxSuccess txHash provider
