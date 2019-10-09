module SignalMarket.Common.Config.Redis where

import           Data.ByteString                  (ByteString)
import qualified Database.Redis                   as Redis
import           SignalMarket.Common.Config.Utils

rawChangeChannel :: ByteString
rawChangeChannel = "raw_change_channel"

makeRedisConnectInfo :: IO Redis.ConnectInfo
makeRedisConnectInfo = makeConfig $ do
  host <- getEnvVarWithDefault "REDIS_HOST" "localhost"
  port <- Redis.PortNumber <$> readEnvVarWithDefault "REDIS_PORT" 6379
  return Redis.defaultConnectInfo { Redis.connectHost = host
                                  , Redis.connectPort = port
                                  }
