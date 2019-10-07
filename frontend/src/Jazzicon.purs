module Jazzicon where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.DOM (Element)


newtype Seed = Seed Number

generate :: { dimension :: Int, seed :: Seed } -> Effect Element
generate { dimension, seed } = runEffectFn2 generate_ dimension seed

foreign import generate_ :: EffectFn2 Int Seed Element

