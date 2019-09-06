module SignalMarket.Indexer (runIndexer) where

import           Control.Exception
import qualified Katip                              as K
import           SignalMarket.Common.Config.Logging (LogConfig (..),
                                                     mkLogConfig)
import           SignalMarket.Indexer.Config        (mkIndexerConfig)
import           SignalMarket.Indexer.IndexerM      (runIndexerM)
import           System.IO                          (stdout)

-- For logging setup, see
-- https://github.com/Soostone/katip/blob/master/katip/examples/example_lens.hs
runIndexer :: IO ()
runIndexer = do
  logCfg <- mkLogConfig "indexer"
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- mkIndexerConfig logCfg {_logEnv = le}
    runIndexerM cfg $ pure ()
