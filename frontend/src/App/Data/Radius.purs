module App.Data.Radius where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (note')
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
import Network.Ethereum.Web3 (BigNumber, UIntN, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)

newtype Radius = Radius (UIntN S256)
derive instance newtypeRadius :: Newtype Radius _
derive instance genericRadius :: Generic Radius _
instance eqRadius :: Eq Radius where eq = genericEq
instance ordRadius :: Ord Radius where compare = genericCompare
instance showRadius :: Show Radius where show = genericShow

instance encodeJsonRadius :: EncodeJson Radius where
  encodeJson = encodeJson <<< radiusToBigNumber

instance decodeJsonRadius :: DecodeJson Radius where
  decodeJson = decodeJson >=> \bn -> note'
    (\_ -> "Expected To get valid Radius but got: " <> show bn)
      $ radiusFromBigNumber bn

radiusToBigNumber :: Radius -> BigNumber
radiusToBigNumber = un Radius >>> unUIntN

radiusFromBigNumber :: BigNumber -> Maybe Radius
radiusFromBigNumber = uIntNFromBigNumber s256 >>> map Radius
