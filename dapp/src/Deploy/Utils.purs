module Deploy.Utils where

import Prelude

import Chanterelle.Internal.Utils.Web3 (pollTransactionReceipt)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Web3 (HexString, Provider, TransactionReceipt(..), TransactionStatus(..))
import Partial.Unsafe (unsafeCrashWith)

-- @TODO: make an issue to add this to chanterelle (Chanterelle.Test module)
awaitTxSuccess :: forall m. MonadAff m => HexString -> Provider -> m Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> unsafeCrashWith $ "Transaction Failed w/ hash " <> show txHash
