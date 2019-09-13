module SignalMarket.Common.Config.PG where

import Control.Error
import SignalMarket.Common.Config.Utils
import Database.PostgreSQL.Simple (ConnectInfo(..))

mkPGConnectInfo :: ExceptT String IO ConnectInfo
mkPGConnectInfo = do
    host <- getEnvVarWithDefault "PG_HOST" "localhost"
    port<- readEnvVarWithDefault "PG_PORT" 5432
    user <- getEnvVarWithDefault "PG_USER" "postgres"
    password <- getEnvVarWithDefault "PG_PASSWORD" "password"
    database <- getEnvVarWithDefault "PG_DB" "signal-market"
    pure $ ConnectInfo
      { connectHost = host
      , connectPort = port
      , connectUser = user
      , connectPassword = password
      , connectDatabase = database
      }

