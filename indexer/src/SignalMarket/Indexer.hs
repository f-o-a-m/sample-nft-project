module SignalMarket.Indexer (runIndexer) where

import           Control.Exception
import qualified Katip                              as K
import           SignalMarket.Common.Config.Logging (LogConfig (..),
                                                     mkLogConfig)
import           SignalMarket.Indexer.Config        (IndexerConfig(..), mkIndexerConfig)
import           SignalMarket.Indexer.IndexerM      (runIndexerM, IndexerM)
import           System.IO                          (stdout)
import Control.Monad.Reader (ask)
import SignalMarket.Common.Config.Types (Contracts(..), DeployReceipt(..))
import           SignalMarket.Indexer.Class (runWeb3)
import           SignalMarket.Indexer.Utils (makeFilter, makeHandler)
import Network.Ethereum.Contract.Event.MultiFilter
import qualified SignalMarket.Indexer.Events.FoamToken as FoamToken
import qualified SignalMarket.Common.Contracts.FoamToken as FoamToken
import Data.Proxy

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

monitor :: IndexerM ()
monitor = do
  cfg <- ask 
  let
    contracts = indexerCfgContracts cfg
    (window, _) = indexerMultiFilterOpts cfg
    foamTokenAddress = deployReceiptAddress . contractsFoamToken $ contracts
    filters = makeFilter (Proxy :: Proxy FoamToken.Transfer) foamTokenAddress 
           :? NilFilters
    handlers = makeHandler cfg FoamToken.foamTokenTransferH
            :& RNil
  runWeb3 $ multiEventManyNoFilter' filters window handlers