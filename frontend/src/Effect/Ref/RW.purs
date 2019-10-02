module Effect.Ref.RW
  ( Ref
  , RefR
  , RefR'
  , RefW
  , RefW'
  , RefRW
  , split
  , new
  , read
  , modify'
  , modify
  , modify_
  , write
  , module Reexport
  ) where

import Prelude

import Control.Capability (Cap) as Reexport
import Control.Capability (kind Cap)
import Control.Capability as Cap
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Data.Tuple (Tuple(..))


newtype Ref (capabilities :: # Cap) a = Ref (Ref.Ref a)

type RefR = RefR' ()
type RefR' r = Ref (Cap.R' r)

type RefW = RefW' ()
type RefW' r = Ref (Cap.W' r)

type RefRW = Ref (Cap.RW)

-- | Create a new mutable reference containing the specified value.
new :: forall m s. MonadEffect m => s -> m (RefRW s)
new x = liftEffect $ Ref <$> Ref.new x

-- | Splits a Read/Write Ref into separate Read and Write Refs.
split :: ∀ a. RefRW a → Tuple (RefR a) (RefW a)
split (Ref r) = Tuple (Ref r) (Ref r)

-- | Read the current value of a mutable reference
read :: forall m s c. MonadEffect m => RefR' c s -> m s
read (Ref r) = liftEffect $ Ref.read r

-- | Update the value of a mutable reference by applying a function
-- | to the current value.
modify'
  :: forall m s b
  . MonadEffect m
  => (s -> { state :: s, value :: b })
  -> RefRW s
  -> m b
modify' f (Ref r) = liftEffect $ Ref.modify' f r

-- | Update the value of a mutable reference by applying a function
-- | to the current value. The updated value is returned
modify
  :: forall m s
  . MonadEffect m
  => (s -> s)
  -> RefRW s
  -> m s
modify f (Ref r) = liftEffect $ Ref.modify f r

-- | Update the value of a mutable reference by applying a function
-- | to the current value.
modify_
  :: forall m s
  . MonadEffect m
  => (s -> s)
  -> RefRW s
  -> m Unit
modify_ f (Ref r) = liftEffect $ Ref.modify_ f r

-- | Update the value of a mutable reference to the specified value.
write
  :: forall m s c
  . MonadEffect m
  => s
  -> RefW' c s
  -> m Unit
write s (Ref r) = liftEffect $ Ref.write s r
