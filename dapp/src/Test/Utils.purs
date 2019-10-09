module Test.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array (intercalate)
import Data.Array.NonEmpty as NAE
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff, Fiber, joinFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Class.Console as C
import Effect.Exception (Error, error)
import Network.Ethereum.Web3 (class KnownSize, BytesN, CallError, Change(..), DLProxy, EventAction(..), Filter, HexString, Provider, UIntN, Web3, Web3Error, embed, event, forkWeb3, fromByteString, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Network.Ethereum.Web3.Solidity.Sizes (S256, S32, s256, s32)
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)
import Test.Spec (ComputationType(..), SpecT, hoistSpec)

type Logger m = String -> m Unit

type TestEnv m =
  { logger :: Logger m -- @NOTE: use this for proper parallel logging
  , signalAttrGen :: m { geohash :: BytesN S32
                       , radius :: UIntN S256
                       }
  }

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

expectWeb3
  :: forall a.
     Show a
  => String
  -> Provider
  -> Web3 a
  -> Aff a
expectWeb3 dbgMsg provider action = do
  res <- runWeb3 provider action
  log $ dbgMsg <> ", result: " <> show res
  expectRight res

expectRight
  :: forall a m b.
     MonadError Error m
  => Show a
  => Either a b
  -> m b
expectRight = case _ of
  Left l -> throwError $ error $ "Expected Right but got (Left " <> show l <>")"
  Right r -> pure r

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

-- special event listener
monitorUntil
  :: forall m e i ni.
     MonadAff m
  => DecodeEvent i ni e
  => Provider
  -> Web3 HexString
  -> Filter e
  -> m e
monitorUntil provider action filter = liftAff do
  valueV <- AVar.empty
  txHashV <- AVar.empty
  -- find the right trx via its hash
  let handler eventValue = do
        (Change e) <- ask
        txHash <- liftAff $ AVar.read txHashV
        if e.transactionHash == txHash
          then do
            liftAff $ AVar.put eventValue valueV
            pure TerminateEvent
          else pure ContinueEvent
  -- start filter
  fiber <- forkWeb3 provider do
    event filter handler
  -- launch action and record its txHash
  txHash <- assertWeb3 provider action
  AVar.put txHash txHashV
  -- join the fiber
  _ <- joinWeb3Fork fiber
  AVar.take valueV

joinWeb3Fork
  :: forall a m.
     MonadAff m
  => Fiber (Either Web3Error a)
  -> m a
joinWeb3Fork fiber = liftAff do
  eRes <- joinFiber fiber
  case eRes of
    Left e -> unsafeCrashWith $ "Error in forked web3 process " <> show e
    Right a -> pure a

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

mkSignalAttrGen
  :: forall m.
     MonadAff m
  => AVar.AVar Int
  -> m { geohash :: BytesN S32
       , radius :: UIntN S256
       }
mkSignalAttrGen uIntV = liftAff do
  firstAvailable <- AVar.take uIntV
  let nextVal = firstAvailable + 1
  AVar.put nextVal uIntV
  pure $ { geohash: mkBytesN s32 $ show firstAvailable
         , radius: mkUIntN s256 firstAvailable
         }

