module Unsafe where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust msg = case _ of
  Nothing -> unsafeCrashWith $ "unsafeFromJust: " <> msg
  Just a -> a

unsafeFromRight :: forall e a. Show e => String -> Either e a -> a
unsafeFromRight msg = case _ of
  Left e -> unsafeCrashWith $ "unsafeFromRight: " <> msg <> "; Left " <> show e
  Right a -> a
