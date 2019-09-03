module Utils where

import Prelude

import Chanterelle.Internal.Utils.Web3 (pollTransactionReceipt)
import Chanterelle.Test (assertWeb3)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Network.Ethereum.Web3 (class KnownSize, BytesN, CallError, DLProxy, HexString, Provider, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, embed, fromByteString, uIntNFromBigNumber)
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)

-- @TODO: make an issue to add this to chanterelle (Chanterelle.Test module)
awaitTxSuccess :: HexString -> Provider -> Aff Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> unsafeCrashWith $ "Transaction Failed w/ hash " <> show txHash

assertStorageCall
  :: forall m a.
     MonadAff m
  => Provider
  -> Web3 (Either CallError a)
  -> m a
assertStorageCall p f = liftAff do
  eRes <- assertWeb3 p f
  case eRes of
    Right x -> pure x
    Left err -> unsafeCrashWith $
                "expected Right in `assertStorageCall`, got error" <> show err

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

mkBytesN
  :: forall n.
     KnownSize n
     => DLProxy n
  -> String
  -> BytesN n
mkBytesN p s = unsafePartialBecause "I know how to make Bytes" $
               fromJust $ fromByteString p =<< flip BS.fromString BS.Hex s

