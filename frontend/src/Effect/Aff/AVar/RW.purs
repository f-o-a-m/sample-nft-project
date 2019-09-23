-- | This module is identical to [Effect.Aff.AVar][Effect.Aff.AVar] with some differences:
-- | - all functions are lifted (i.e. with MonadAff or MonadEffect constraint)
-- | - defines `AVar` which is a newtype over regular `Effect.AVar` with extra
-- |   addition of `Cap`abilities, in the same way as in [Effect.Aff.Bus][Effect.Aff.Bus].
-- |   so we can understand how particular `Avar` can be used. here are some examples of function types and
-- |   function types and what the function can do with the an AVar.
-- |   - `f :: forall r. AVarW' r -> Aff _ Unit` can write only (put,tryPut, kill)
-- |   - `f :: forall r. AVarR' r -> Aff _ Unit` can read only (read, tryRead, status, isEmptyVar, isFilledVar, isKilledVar)
-- |   - `f :: AVarRW -> Aff _ Unit` can do everything `AVarW' ()` and `AVarR ()` can do, plus take and tryTakeV
-- |   - `f :: forall r. AVar' r -> Aff _ Unit` can do nothing except passing around or storing somewhere
-- |
-- | [Effect.Aff.AVar]: https://github.com/slamdata/purescript-aff/blob/v4.1.1/src/Control/Monad/Aff/AVar.purs
-- | [Effect.Aff.Bus]: https://github.com/slamdata/purescript-aff-bus/blob/v3.1.0/src/Control/Monad/Aff/Bus.purs
module Effect.Aff.AVar.RW
  ( module Effect.AVar
  , AVar
  , AVarR
  , AVarR'
  , AVarW
  , AVarW'
  , AVarRW
  , split
  , new
  , empty
  , status
  , take
  , tryTake
  , put
  , tryPut
  , read
  , tryRead
  , kill
  , module Reexport
  ) where

import Prelude

import Control.Capability (kind Cap)
import Control.Capability (Cap) as Reexport
import Control.Capability as Cap
import Effect.Aff.AVar (isEmpty, isFilled, isKilled) as Reexport
import Effect.Aff.AVar as AffAvar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.AVar (AVarStatus(..), isEmpty, isFilled, isKilled)
import Effect.AVar as AVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

newtype AVar (capabilities :: # Cap) a = AVar (AVar.AVar a)

type AVarR = AVarR' ()
type AVarR' r = AVar (Cap.R' r)

type AVarW = AVarW' ()
type AVarW' r = AVar (Cap.W' r)

type AVarRW = AVar (Cap.RW)

-- | Splits a Read/Write AVar into separate Read and Write AVars.
split :: ∀ a. AVarRW a → Tuple (AVarR a) (AVarW a)
split (AVar v) = Tuple (AVar v) (AVar v)

-- | Creates a fresh AVar with an initial value.
new :: forall m a. MonadEffect m => a -> m (AVarRW a)
new var = liftEffect $ AVar <$> AVar.new var

-- | Creates a fresh AVar.
empty :: forall m a. MonadEffect m => m (AVarRW a)
empty = liftEffect $ AVar <$> AVar.empty

-- | Synchronously checks the status of an AVar.
status :: forall m a r. MonadEffect m => AVarR' r a -> m (AVar.AVarStatus a)
status (AVar var) = liftEffect $ AVar.status var

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills.
take :: forall m a. MonadAff m => AVarRW a -> m a
take (AVar var) = liftAff $ AffAvar.take var

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake :: forall m a. MonadEffect m => AVarRW a -> m (Maybe a)
tryTake (AVar var) = liftEffect $ AVar.tryTake var

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available.
put :: forall m a r. MonadAff m => a -> AVarW' r a -> m Unit
put value (AVar avar) = liftAff $ AffAvar.put value avar

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut :: forall m a r. MonadEffect m => a -> AVarW' r a -> m Boolean
tryPut value (AVar avar) = liftEffect $ AVar.tryPut value avar

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read :: forall m a r. MonadAff m => AVarR' r a -> m a
read (AVar var) = liftAff $ AffAvar.read var

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead :: forall m a r. MonadEffect m => AVarR' r a -> m (Maybe a)
tryRead (AVar var) = liftEffect $ AVar.tryRead var

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill :: forall m a r. MonadEffect m => Error -> AVarW' r a -> m Unit
kill error (AVar avar) = liftEffect $ AVar.kill error avar
