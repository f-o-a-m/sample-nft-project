module SignalMarket.Server (runServer) where

import           Data.Monoid                          (Endo (..))
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           SignalMarket.Server.Config.Utils     (makeConfig,
                                                       readEnvVarWithDefault)
import           SignalMarket.Server.Server           (mkApplication)

runServer :: IO ()
runServer = do
  port <- makeConfig $ readEnvVarWithDefault "SERVER_PORT" 9000
  app <- mkApplication
  let appWithMiddleware = addMiddleware app
  run port appWithMiddleware
  where
    addMiddleware = appEndo . mconcat . map Endo $
      [ logStdoutDev
      , gzip def
      ]
