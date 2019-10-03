module App.Data.Radius where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

newtype Radius = Radius Number
derive instance newtypeRadius :: Newtype Radius _
derive instance genericRadius :: Generic Radius _
instance eqRadius :: Eq Radius where eq = genericEq
instance ordRadius :: Ord Radius where compare = genericCompare
instance showRadius :: Show Radius where show = genericShow
derive newtype instance encodeJsonRadius :: EncodeJson Radius
derive newtype instance decodeJsonRadius :: DecodeJson Radius