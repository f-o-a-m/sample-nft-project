module SignalMarket.Indexer (runIndexer) where

import           Control.Exception
import           Control.Monad.Reader                        (ask)
import           Data.Proxy
import qualified Katip                                       as K
import           Network.Ethereum.Contract.Event.MultiFilter
import           SignalMarket.Common.Config.Logging          (LogConfig (..),
                                                              mkLogConfig)
import           SignalMarket.Common.Config.Types            (Contracts (..),
                                                              DeployReceipt (..))
import qualified SignalMarket.Common.Contracts.FoamToken     as FoamToken
import           SignalMarket.Indexer.Class                  (runWeb3)
import           SignalMarket.Indexer.Config                 (IndexerConfig (..),
                                                              mkIndexerConfig)
import qualified SignalMarket.Indexer.Events.FoamToken       as FoamToken
import           SignalMarket.Indexer.IndexerM               (IndexerM,
                                                              runIndexerM)
import           SignalMarket.Indexer.Utils                  (makeFilterHandlerPair)
import           System.IO                                   (stdout)

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

  -- FOAM Token events
  (ftTransferF, ftTransferH) <- makeFilterHandlerPair foamTokenAddress FoamToken.foamTokenTransferH

  let
    filters = ftTransferF
           :? NilFilters
    handlers = ftTransferH
            :& RNil
  runWeb3 $ multiEventManyNoFilter' filters window handlers
