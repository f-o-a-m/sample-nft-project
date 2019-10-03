module  App.HTML.Canceler where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Canceler(..), error, launchAff_)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import React.Basic (ReactComponentInstance)
import React.Basic as React

foreign import readCanceler :: forall p s. EffectFn1 (ReactComponentInstance p s) (Nullable Canceler)
foreign import setCanceler :: forall p s. EffectFn2 (Nullable Canceler) (ReactComponentInstance p s) Unit

runCancelers :: forall p s . React.Self p s -> Effect Unit
runCancelers {instance_} = do
  canceler <- runEffectFn1 readCanceler instance_ <#> toMaybe >>> fromMaybe mempty
  launchAff_ $ un Canceler canceler $ error "runCancelers"
  runEffectFn2 setCanceler (toNullable $ Nothing) instance_

pushCanceler :: forall p s . React.Self p s -> Canceler -> Effect Unit
pushCanceler {instance_} c = do
  canceler <- runEffectFn1 readCanceler instance_ <#> toMaybe >>> fromMaybe mempty
  runEffectFn2 setCanceler (toNullable $ Just $ canceler <> c) instance_
