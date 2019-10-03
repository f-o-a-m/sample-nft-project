module Data.Number.Extras where


import Data.Int (Radix)
import Data.Maybe (Maybe(..))

-- or we could use `toStringWith (Fixed __ ) __`` from https://github.com/sharkdp/purescript-numbers/blob/master/src/Data/Number/Format.purs#L61
-- but that's as unsafe as this one (toFixed can throw)
foreign import toFixed ∷ Int → Number → String

integerFromStringAs :: Radix -> String -> Maybe Number
integerFromStringAs = fromStringAsImpl Just Nothing

foreign import fromStringAsImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Radix
  -> String
  -> Maybe Number

