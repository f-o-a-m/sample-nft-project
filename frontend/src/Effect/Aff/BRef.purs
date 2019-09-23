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


module Effect.Aff.BRef
  ( BRef
  , BRefR
  , BRefR'
  , BRefW
  , BRefW'
  , BRefRW
  , make
  , readOnUpdate
  , read
  , write
  , split
  , kill
  ) where

import Prelude

import Control.Capability (kind Cap)
import Control.Capability as Cap
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exn
import Effect.Ref.RW as Ref
import Data.Tuple (Tuple(..))

newtype BRef (r :: # Cap) a = BRef
  { bus :: Bus.Bus r Unit
  , ref :: Ref.Ref r a
  }

type BRefR = BRefR' ()
type BRefR' r = BRef (Cap.R' r)

type BRefW = BRefW' ()
type BRefW' r = BRef (Cap.W' r)

type BRefRW = BRef Cap.RW

make :: forall m a. MonadEffect m => a -> m (BRefRW a)
make a = liftEffect do
  bus ← Bus.make
  ref ← Ref.new a
  pure $ BRef { ref, bus }

readOnUpdate :: forall r m a. MonadAff m => BRefR' r a -> m a
readOnUpdate (BRef { ref, bus }) = liftAff do
  Bus.read bus
  liftEffect $ Ref.read ref

read :: forall r m a. MonadEffect m => BRefR' r a -> m a
read (BRef { ref }) = liftEffect do
  -- TODO (safareli) if is killed `read` should resolve with error.
  Ref.read ref

write :: forall r m a. MonadAff m => a -> BRefW' r a -> m Unit
write a (BRef { ref, bus}) = liftAff do
  liftEffect $ Ref.write a ref
  Bus.write unit bus

split :: ∀ a. BRefRW a -> Tuple (BRefR a) (BRefW a)
split (BRef {bus, ref}) =
  let
    Tuple busR busW = Bus.split bus
    Tuple refR refW = Ref.split ref
  in Tuple (BRef {bus:busR, ref:refR}) (BRef {bus: busW, ref: refW})


kill :: forall r m a. MonadAff m => Exn.Error -> BRefW' r a -> m Unit
kill err (BRef {bus}) = liftAff $ Bus.kill err bus
