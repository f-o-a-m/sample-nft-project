module Test.Utils where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array (intercalate)
import Data.Array.NonEmpty as NAE
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (class KnownSize, BytesN, CallError, DLProxy, Provider, UIntN, Web3, embed, fromByteString, runWeb3, uIntNFromBigNumber)
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)
import Test.Spec (ComputationType(..), SpecT, hoistSpec)

type Logger m = String -> m Unit

go :: SpecT (ReaderT (Logger Aff) Aff) Unit Aff ~> SpecT Aff Unit Aff
go = hoistSpec identity \cType m ->
  let
    prefix = case cType of
      CleanUpWithContext n -> intercalate " > " n <> " (afterAll) "
      TestWithName n -> intercalate " > " $ NAE.toArray n
  in runReaderT m \logMsg -> C.log $ prefix  <> "| " <> logMsg

assertWeb3Test
  :: forall m a.
     MonadAff m
  => Provider
  -> Web3 a
  -> m a
assertWeb3Test provider a = liftAff $ runWeb3 provider a <#> case _ of
  Right x -> x
  Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

assertStorageCall
  :: forall m a.
     MonadAff m
  => Provider
  -> Web3 (Either CallError a)
  -> m a
assertStorageCall p f = liftAff do
  eRes <- assertWeb3Test p f
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
