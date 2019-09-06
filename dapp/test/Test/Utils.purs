module Test.Utils where

import Prelude

import Contracts.SignalToken as SignalToken
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array ((..), intercalate)
import Data.Array.NonEmpty as NAE
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff, Fiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (class KnownSize, BytesN, CallError, DLProxy, EventAction(..), Filter, Provider, UIntN, Web3, Web3Error, _fromBlock, _toBlock, embed, event', forkWeb3, fromByteString, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, S32, s256, s32)
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

assertWeb3
  :: forall m a.
     MonadAff m
  => Provider
  -> Web3 a
  -> m a
assertWeb3 provider a = liftAff $ runWeb3 provider a <#> case _ of
  Right x -> x
  Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

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

-- special event listener for unique SignalTokens
monitorUntil
  :: forall m.
     MonadAff m
  => Provider
  -> Logger m
  -> Filter SignalToken.TrackedToken
  -> (SignalToken.TrackedToken -> Boolean)
  -> m
       ( Fiber
         ( Either Web3Error
             { valueV :: AVar.AVar SignalToken.TrackedToken
             }
         )
       )
monitorUntil provider logger filter pred = do
  valueV <- liftAff $ AVar.empty
  logger $ "Creating filter with fromBlock=" <>
    show (filter ^. _fromBlock) <> " toBlock=" <> show (filter ^. _toBlock)
  let handler token@(SignalToken.TrackedToken s) = do
        let shouldTerminate = pred token
        if shouldTerminate
          then do
            liftAff $ AVar.put token valueV
            pure TerminateEvent
          else pure ContinueEvent
  liftAff $ forkWeb3 provider $ do
    _ <- event' filter 0 handler
    pure { valueV }

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


mkUIntsGen
  :: forall m.
     MonadAff m
  => AVar.AVar Int
  -> Int
  -> m (Array (UIntN S256))
mkUIntsGen uintV n = liftAff do
  firstAvailable <- AVar.take uintV
  let nextVal = firstAvailable + n
      res = firstAvailable .. (nextVal - 1)
  AVar.put nextVal uintV
  pure $ map (mkUIntN s256) res

mkBytesGen
  :: forall m.
     MonadAff m
  => AVar.AVar Int
  -> Int
  -> m (Array (BytesN S32))
mkBytesGen byteV n = liftAff do
  firstAvailable <- AVar.take byteV
  let nextVal = firstAvailable + n
      res = firstAvailable .. (nextVal - 1)
  AVar.put nextVal byteV
  pure $ map (mkBytesN s32 <<< show) res
