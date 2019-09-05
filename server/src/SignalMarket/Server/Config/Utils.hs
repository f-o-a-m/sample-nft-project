module SignalMarket.Server.Config.Utils
  ( getEnvVar
  , getEnvVarWithDefault
  , readEnvVar
  , readEnvVarWithDefault
  , getContractAddress
  , makeConfig
  ) where

import           Control.Error
import           Control.Lens               ((^?))
import           Control.Monad.IO.Class
import qualified Data.Aeson.Lens            as AL
import           Data.Solidity.Prim.Address (Address)
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import           System.Environment
import qualified Text.Read                  as T

-- | Grab an environment variable and attempt to parse it via its Read instance.
readEnvVar :: (Read a, MonadIO m) => String -> ExceptT String m a
readEnvVar var = do
  str <- liftIO (lookupEnv var) !? ("Missing Environment Variable: " ++ var)
  T.readMaybe str ?? ("Couldn't Parse Environment Variable " ++ var ++ ": " ++ str)

-- | Grab an environment variable. If it exists, attempt to parse it via its Read instance.
-- | Otherwise, use the supplied fallback.
readEnvVarWithDefault :: (Read a, Show a, MonadIO m) => String -> a -> ExceptT String m a
readEnvVarWithDefault var def = liftIO (lookupEnv var) >>= \case
    Nothing -> do
      liftIO $ print $ "Defaulting " <> show var <> " to " <> show def <> " as it was not set in the environment"
      return def
    Just s -> T.readMaybe s ?? ("Couldn't Parse Environment Variable " ++ var ++ ": " ++ s)

-- | Grab an environment variable as a String
getEnvVar :: MonadIO m => String -> ExceptT String m String
getEnvVar var = liftIO (lookupEnv var) !? ("Missing Environment Variable: " ++ var)

-- | Grab an environment variable as a String, falling back to the supplied
-- | default if its not set.
getEnvVarWithDefault :: MonadIO m => String -> String -> ExceptT String m String
getEnvVarWithDefault var def = liftIO (lookupEnv var) >>= \case
  Nothing -> do
    liftIO $ print $ "Defaulting " <> show var <> " to " <> show def <> " as it was not set in the environment"
    return def
  Just x -> return x

parseAddressFromABI :: String -> String -> Maybe Address
parseAddressFromABI abi networkID =
  abi ^? AL.key "networks"
       . AL.key (cs networkID)
       . AL.key "address"
       . AL._JSON

getContractAddress :: MonadIO m => String -> String -> ExceptT String m Address
getContractAddress contractName networkID = do
    abisDir <- getEnvVarWithDefault "ABIS_DIR" "./build"
    let abiPath = abisDir <> "/" <> contractName <> ".json"
    abi <- liftIO $ readFile abiPath
    parseAddressFromABI abi networkID ??
      ("Couldn't parse address from ABI at " <> abiPath <> " with networkID " <> cs networkID)


makeConfig :: MonadIO m => ExceptT String m a -> m a
makeConfig a = do
  econf <- runExceptT a
  either error return econf
