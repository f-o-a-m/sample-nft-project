module SignalMarket.Common.Config.PG where

import           Control.Error
import           Database.PostgreSQL.Simple       (ConnectInfo (..))
import           SignalMarket.Common.Config.Utils

-- | ConnectInfo can be turned into a bonafide connection
-- | using 'Database.PostgreSQL.Simple.connect'.
mkPGConnectInfo :: ExceptT String IO ConnectInfo
mkPGConnectInfo = do
    host <- getEnvVarWithDefault "PGHOST" "localhost"
    port<- readEnvVarWithDefault "PGPORT" 5432
    user <- getEnvVarWithDefault "PGUSER" "postgres"
    password <- getEnvVarWithDefault "PGPASSWORD" "password"
    database <- getEnvVarWithDefault "PGDATABASE" "signal_market"
    pure $ ConnectInfo
      { connectHost = host
      , connectPort = port
      , connectUser = user
      , connectPassword = password
      , connectDatabase = database
      }

