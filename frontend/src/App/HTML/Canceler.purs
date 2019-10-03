module  App.HTML.Canceler where


import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Canceler(..), error, launchAff_)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import React.Basic (ReactComponentInstance)
import React.Basic as React

foreign import readCanceler :: forall p s. EffectFn2 String (ReactComponentInstance p s) (Nullable Canceler)
foreign import setCanceler :: forall p s. EffectFn3 String (Nullable Canceler) (ReactComponentInstance p s) Unit


runCancelers :: forall p s . React.Self p s -> Effect Unit
runCancelers = runCancelers' "__default__"

runCancelers' :: forall p s . String -> React.Self p s -> Effect Unit
runCancelers' key {instance_} = do
  canceler <- runEffectFn2 readCanceler key instance_ <#> toMaybe >>> fromMaybe mempty
  launchAff_ $ un Canceler canceler $ error "runCancelers"
  runEffectFn3 setCanceler key (toNullable $ Nothing) instance_

pushCanceler :: forall p s . React.Self p s -> Canceler -> Effect Unit
pushCanceler = pushCanceler' "__default__"

pushCanceler' :: forall p s . String -> React.Self p s -> Canceler -> Effect Unit
pushCanceler' key {instance_} c = do
  canceler <- runEffectFn2 readCanceler key instance_ <#> toMaybe >>> fromMaybe mempty
  runEffectFn3 setCanceler key (toNullable $ Just $ canceler <> c) instance_
