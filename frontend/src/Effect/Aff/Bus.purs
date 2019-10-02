{-
Copyright 2018 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

-- | This version is from: safareli/purescript-aff-bus#v3.0.1-kill
-- | which uses Cap from Control.Capability instead of defining it here
module Effect.Aff.Bus
  ( make
  , read
  , write
  , split
  , kill
  , isKilled
  , Bus
  , BusRW
  , BusR
  , BusR'
  , BusW
  , BusW'
  , module Reexport
  ) where

import Prelude

import Control.Capability (Cap) as Reexport
import Control.Capability (kind Cap)
import Control.Capability as Cap
import Effect.Aff (attempt, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.AVar as EffAvar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exn
import Data.Foldable (foldl, sequence_, traverse_)
import Data.List (List, (:))
import Data.Tuple (Tuple(..))

data Bus (r :: # Cap) a = Bus (AVar a) (AVar (List (AVar a)))

type BusR = BusR' ()
type BusR' r = Bus (Cap.R' r)

type BusW = BusW' ()
type BusW' r = Bus (Cap.W' r)

type BusRW = Bus Cap.RW

-- | Creates a new bidirectional Bus which can be read from and written to.
make :: ∀ m a. MonadEffect m ⇒ m (BusRW a)
make = liftEffect do
  cell ← EffAvar.empty
  consumers ← EffAvar.new mempty
  let
    loop = attempt (AVar.take cell) >>= traverse_ \res -> do
      vars ← AVar.take consumers
      AVar.put mempty consumers
      sequence_ (foldl (\xs a -> AVar.put res a : xs) mempty vars)
      loop
  launchAff_ loop

  pure $ Bus cell consumers

-- | Blocks until a new value is pushed to the Bus, returning the value.
read :: ∀ m a r. MonadAff m => BusR' r a -> m a
read (Bus _ consumers) = liftAff do
  res' ← AVar.empty
  cs ← AVar.take consumers
  AVar.put (res' : cs) consumers
  AVar.take res'

-- | Pushes a new value to the Bus, yielding immediately.
write :: ∀ m a r. MonadAff m => a -> BusW' r a -> m Unit
write a (Bus cell _) = liftAff $ AVar.put a cell

-- | Splits a bidirectional Bus into separate read and write Buses.
split :: ∀ a. BusRW a -> Tuple (BusR a) (BusW a)
split (Bus a b) = Tuple (Bus a b) (Bus a b)

-- | Kills the Bus and propagates the exception to all pending and future consumers.
kill :: ∀ m a r. MonadAff m => Exn.Error -> BusW' r a -> m Unit
kill err (Bus cell consumers) = liftAff $ unlessM (liftEffect $ EffAvar.isKilled <$> EffAvar.status cell) do
  AVar.kill err cell
  vars ← AVar.take consumers
  traverse_ (AVar.kill err) vars
  AVar.kill err consumers

-- | Synchronously checks whether a Bus has been killed.
isKilled :: ∀ m a r. MonadEffect m ⇒ BusR' r a -> m Boolean
isKilled (Bus cell _) = liftEffect $ EffAvar.isKilled <$> EffAvar.status cell
