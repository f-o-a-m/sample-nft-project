module Deploy.Utils where

import Prelude

import Chanterelle.Internal.Utils.Web3 (pollTransactionReceipt)
import Contracts.SignalMarket (signalToSale)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Web3 (class KnownSize, type (:&), Address, CallError(..), ChainCursor, D2, D5, D6, DLProxy, DOne, HexString, Provider, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, embed, mkAddress, mkHexString, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity (Tuple4(..))
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)

-- @TODO: make an issue to add this to chanterelle (Chanterelle.Test module)
awaitTxSuccess :: forall m. MonadAff m => HexString -> Provider -> m Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> unsafeCrashWith $ "Transaction Failed w/ hash " <> show txHash

-- safeSignalToSale
-- FFI function wrapped to avoid defaulting behavior
safeSignalToSale :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Address))
safeSignalToSale txOpts cc tokenId = do
  eRes <- signalToSale txOpts cc tokenId
  pure case eRes of
    Left e -> Left e
    Right a -> if a == nullSale
               then Left $ NullStorageError { signature: ""
                                            , _data: mempty
                                            }
               else Right a

-- the default value
nullSale :: (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Address)
nullSale = Tuple4 nullUIntValue nullUIntValue nullUIntValue nullAddress
  where nullUIntValue = mkUIntN s256 0
        nullAddress = unsafeFromJust "Must be valid Address 000..." $
                      mkAddress =<< mkHexString "0x0000000000000000000000000000000000000000"

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust msg = case _ of
  Nothing -> unsafeCrashWith $ "unsafeFromJust: " <> msg
  Just a -> a

mkUIntN
  :: forall n.
     KnownSize n
  => DLProxy n
  -> Int
  -> UIntN n
mkUIntN p n = unsafePartialBecause "I know how to make a UInt" $
              fromJust $ uIntNFromBigNumber p $ embed n
