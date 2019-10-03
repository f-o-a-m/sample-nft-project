module SignalMarket.Server (runServer) where

import           Control.Exception
import           Data.Monoid                          (Endo (..))
import qualified Katip                                as K
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (cors, corsMethods,
                                                       corsRequestHeaders,
                                                       simpleCorsResourcePolicy,
                                                       simpleHeaders,
                                                       simpleMethods)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           SignalMarket.Common.Config.Logging   (LogConfig (..),
                                                       mkLogConfig)
import           SignalMarket.Common.Config.Utils     (makeConfig,
                                                       readEnvVarWithDefault)
import           SignalMarket.Server.Config           (mkAppConfig)
import           SignalMarket.Server.Server           (mkApplication)
import           System.IO                            (stdout)

-- For logging setup, see
-- https://github.com/Soostone/katip/blob/master/katip/examples/example_lens.hs
runServer :: IO ()
runServer = do
  port <- makeConfig $ readEnvVarWithDefault "SERVER_PORT" 9000
  logCfg <- mkLogConfig "server"
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- mkAppConfig logCfg {_logEnv = le}
    let app = mkApplication cfg
        appWithMiddleware = addMiddleware app
    run port appWithMiddleware
  where
    addMiddleware = appEndo . mconcat . map Endo $
      [ logStdoutDev
      , gzip def
      , cors (const $ Just $ simpleCorsResourcePolicy
          { corsRequestHeaders = "Authorization" : simpleHeaders
          , corsMethods = "DELETE" : simpleMethods
          }) -- cors policy
      ]
