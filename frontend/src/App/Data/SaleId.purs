module App.Data.SaleId where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (note')
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
import Network.Ethereum.Web3 (BigNumber, UIntN, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)



newtype SaleId = SaleId (UIntN S256)
derive instance newtypeSaleId :: Newtype SaleId _
derive instance genericSaleId :: Generic SaleId _
instance eqSaleId :: Eq SaleId where eq = genericEq
instance ordSaleId :: Ord SaleId where compare = genericCompare
instance showSaleId :: Show SaleId where show = genericShow

instance encodeJsonSaleId :: EncodeJson SaleId where
  encodeJson = encodeJson <<< saleIdToBigNumber

instance decodeJsonSaleId :: DecodeJson SaleId where
  decodeJson = decodeJson >=> \bn -> note'
    (\_ -> "Expected To get valid SaleId but got: " <> show bn)
      $ saleIdFromBigNumber bn

saleIdToBigNumber :: SaleId -> BigNumber
saleIdToBigNumber = un SaleId >>> unUIntN

saleIdFromBigNumber :: BigNumber -> Maybe SaleId
saleIdFromBigNumber = uIntNFromBigNumber s256 >>> map SaleId