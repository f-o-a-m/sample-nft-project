module App.Data.Token where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (note')
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
import Network.Ethereum.Web3 (BigNumber, UIntN, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3 as BN
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Unsafe (unsafeFromJust)

newtype Token = Token (UIntN S256)
derive instance newtypeToken :: Newtype Token _
derive instance genericToken :: Generic Token _
instance eqToken :: Eq Token where eq = genericEq
instance ordToken :: Ord Token where compare = genericCompare
instance showToken :: Show Token where show = genericShow

instance encodeJsonToken :: EncodeJson Token where
  encodeJson = encodeJson <<< tokenToBigNumber

instance decodeJsonToken :: DecodeJson Token where
  decodeJson = decodeJson >=> \bn -> note'
    (\_ -> "Expected To get valid Token but got: " <> show bn)
      $ tokenFromBigNumber bn

zeroToken :: Token
zeroToken = unsafeFromJust "0 is valid Token" $ tokenFromBigNumber $ BN.embed 0

tokenToBigNumber :: Token -> BigNumber
tokenToBigNumber = un Token >>> unUIntN

tokenFromBigNumber :: BigNumber -> Maybe Token
tokenFromBigNumber = uIntNFromBigNumber s256 >>> map Token

addTokens :: Token -> Token -> Maybe Token
addTokens a b = tokenFromBigNumber $ on (+) tokenToBigNumber a b
