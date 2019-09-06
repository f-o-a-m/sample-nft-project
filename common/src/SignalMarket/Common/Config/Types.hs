module SignalMarket.Common.Config.Types
    ( Contracts(..)
    , DeployReceipt(..)
    , mkContracts
    ) where

import           Control.Error
import           Control.Lens                     ((^?))
import           Control.Monad.IO.Class
import qualified Data.Aeson                       as AE
import qualified Data.Aeson.Lens                  as AEL
import           Data.ByteArray.HexString         (HexString)
import           Data.Solidity.Prim.Address       (Address)
import           Data.String.Conversions          (cs)
import           GHC.Generics                     (Generic)
import           SignalMarket.Common.Aeson
import           SignalMarket.Common.Config.Utils

data Contracts = Contracts
  { contractsFoamToken    :: DeployReceipt
  , contractsSignalToken  :: DeployReceipt
  , contractsSignalMarket :: DeployReceipt
  } deriving (Eq, Show, Generic)

contractsAesonOptions :: AE.Options
contractsAesonOptions = defaultAesonOptions "contracts"

instance AE.FromJSON Contracts where
    parseJSON = AE.genericParseJSON contractsAesonOptions

instance AE.ToJSON Contracts where
    toJSON = AE.genericToJSON contractsAesonOptions

mkContracts :: String -> ExceptT String IO Contracts
mkContracts networkID = do
  foamToken <- getDeployReceipt "FoamToken" networkID
  signalToken <- getDeployReceipt "SignalToken" networkID
  signalMarket <- getDeployReceipt "SignalMarket" networkID
  return $ Contracts
    { contractsFoamToken = foamToken
    , contractsSignalToken = signalToken
    , contractsSignalMarket = signalMarket
    }

data DeployReceipt = DeployReceipt
  { deployReceiptAddress         :: Address
  , deployReceiptBlockHash       :: HexString
  , deployReceiptBlockNumber     :: HexString
  , deployReceiptTransactionHash :: HexString
  } deriving (Eq, Show, Generic)

deployReceiptAesonOptions :: AE.Options
deployReceiptAesonOptions = defaultAesonOptions "deployReceipt"

instance AE.FromJSON DeployReceipt where
    parseJSON = AE.genericParseJSON deployReceiptAesonOptions

instance AE.ToJSON DeployReceipt where
    toJSON = AE.genericToJSON deployReceiptAesonOptions

parseDeployReceiptFromABI :: String -> String -> Maybe DeployReceipt
parseDeployReceiptFromABI abi networkID =
  abi ^? AEL.key "networks"
       . AEL.key (cs networkID)
       . AEL._JSON

getDeployReceipt :: MonadIO m => String -> String -> ExceptT String m DeployReceipt
getDeployReceipt contractName networkID = do
    abisDir <- getEnvVarWithDefault "ABIS_DIR" "./build"
    let abiPath = abisDir <> "/" <> contractName <> ".json"
    abi <- liftIO $ readFile abiPath
    parseDeployReceiptFromABI abi networkID ??
      ("Couldn't parse address from ABI at " <> abiPath <> " with networkID " <> cs networkID)
