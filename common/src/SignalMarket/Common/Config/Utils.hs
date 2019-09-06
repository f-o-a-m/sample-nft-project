module SignalMarket.Common.Config.Utils
  ( getEnvVar
  , getEnvVarWithDefault
  , readEnvVar
  , readEnvVarWithDefault
  , makeConfig
  ) where

import           Control.Error
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           System.Environment
import qualified Text.Read              as T

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

makeConfig :: MonadIO m => ExceptT String m a -> m a
makeConfig a = do
  econf <- runExceptT a
  either error return econf