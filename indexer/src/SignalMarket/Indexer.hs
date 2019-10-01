module SignalMarket.Indexer (runIndexer) where

import           Control.Exception
import           Control.Monad.Reader                        (ask)
import qualified Katip                                       as K
import           Network.Ethereum.Contract.Event.MultiFilter
import           SignalMarket.Common.Class                   (runWeb3)
import           SignalMarket.Common.Config.Logging          (LogConfig (..),
                                                              mkLogConfig)
import           SignalMarket.Common.Config.Types            (Contracts (..))
import           SignalMarket.Indexer.Config                 (IndexerConfig (..),
                                                              mkIndexerConfig)
import qualified SignalMarket.Indexer.Events.FoamToken       as FoamToken
import qualified SignalMarket.Indexer.Events.SignalMarket    as SignalMarket
import qualified SignalMarket.Indexer.Events.SignalToken     as SignalToken
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
    runIndexerM cfg $ monitor

monitor :: IndexerM ()
monitor = do
  K.logFM K.InfoS "Starting Indexer ..."
  cfg <- ask
  let
    contracts = indexerCfgContracts cfg
    (window, _) = indexerMultiFilterOpts cfg
    foamTokenReceipt = contractsFoamToken contracts
    signalTokenReceipt = contractsSignalToken contracts
    signalMarketReceipt = contractsSignalMarket contracts
  -- FOAM Token events
  (ftTransferF, ftTransferH) <- makeFilterHandlerPair foamTokenReceipt FoamToken.foamTokenTransferH
  -- Signal Token events
  (stTransferF, stTransferH) <- makeFilterHandlerPair signalTokenReceipt SignalToken.signalTokenTransferH
  (stTrackedTokenF, stTrackedTokenH) <- makeFilterHandlerPair signalTokenReceipt SignalToken.signalTokenTrackedTokenH
  (stTokensStakedF, stTokensStakedH) <- makeFilterHandlerPair signalTokenReceipt SignalToken.signalTokenTokensStakedH
  (stTokensUnstakedF, stTokensUnstakedH) <- makeFilterHandlerPair signalTokenReceipt SignalToken.signalTokenTokensUnstakedH
  -- Signal Market events
  (smSignalForSaleF, smSignalForSaleH) <- makeFilterHandlerPair signalMarketReceipt SignalMarket.signalMarketSignalForSaleH
  (smSignalSoldF, smSignalSoldH) <- makeFilterHandlerPair signalMarketReceipt SignalMarket.signalMarketSignalSoldH
  let
    filters = ftTransferF
           :? stTransferF
           :? stTrackedTokenF
           :? stTokensStakedF
           :? stTokensUnstakedF
           :? smSignalForSaleF
           :? smSignalSoldF
           :? NilFilters
    handlers = ftTransferH
            :& stTransferH
            :& stTrackedTokenH
            :& stTokensStakedH
            :& stTokensUnstakedH
            :& smSignalForSaleH
            :& smSignalSoldH
            :& RNil
  runWeb3 $ multiEventManyNoFilter' filters window handlers
  K.logFM K.InfoS "Terminating Indexer ..."


