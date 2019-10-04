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
import Network.Ethereum.Web3.Types (ETHER)
import Network.Ethereum.Web3.Types.TokenUnit (kind Token) as T
import Type.Proxy (Proxy)
import Unsafe (unsafeFromJust)

foreign import data FOAM :: T.Token

class TokenName t where
  tokenName :: (Proxy t) -> String
instance ethTokenName :: TokenName (Token ETHER) where tokenName _ = "ETH"
instance foamTokenName :: TokenName (Token FOAM) where tokenName _ = "FOAM"

newtype Token (t :: T.Token) = Token (UIntN S256)
derive instance newtypeToken :: Newtype (Token t) _
derive instance genericToken :: Generic (Token t) _
instance eqToken :: Eq (Token t) where eq = genericEq
instance ordToken :: Ord (Token t) where compare = genericCompare
instance showToken :: Show (Token t) where show = genericShow

instance encodeJsonToken :: EncodeJson (Token t) where
  encodeJson = encodeJson <<< tokenToBigNumber

instance decodeJsonToken :: DecodeJson (Token t) where
  decodeJson = decodeJson >=> \bn -> note'
    (\_ -> "Expected To get valid Token but got: " <> show bn)
      $ tokenFromBigNumber bn

zeroToken :: forall t. (Token t)
zeroToken = unsafeFromJust "0 is valid Token" $ tokenFromBigNumber $ BN.embed 0

tokenToBigNumber :: forall t. Token t -> BigNumber
tokenToBigNumber = un Token >>> unUIntN

tokenFromBigNumber :: forall t. BigNumber -> Maybe (Token t)
tokenFromBigNumber = uIntNFromBigNumber s256 >>> map Token

addTokens :: forall t. Token t -> Token t -> Maybe (Token t)
addTokens a b = tokenFromBigNumber $ on (+) tokenToBigNumber a b
