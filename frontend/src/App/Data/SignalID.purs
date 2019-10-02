module App.Data.SignalId where

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


newtype SignalId = SignalId (UIntN S256)
derive instance newtypeSignalId :: Newtype SignalId _
derive instance genericSignalId :: Generic SignalId _
instance eqSignalId :: Eq SignalId where eq = genericEq
instance ordSignalId :: Ord SignalId where compare = genericCompare
instance showSignalId :: Show SignalId where show = genericShow

instance encodeJsonSignalId :: EncodeJson SignalId where
  encodeJson = encodeJson <<< signalIdToBigNumber

instance decodeJsonSignalId :: DecodeJson SignalId where
  decodeJson = decodeJson >=> \bn -> note'
    (\_ -> "Expected To get valid SignalId but got: " <> show bn)
      $ signalIdFromBigNumber bn

signalIdToBigNumber :: SignalId -> BigNumber
signalIdToBigNumber = un SignalId >>> unUIntN

signalIdFromBigNumber :: BigNumber -> Maybe SignalId
signalIdFromBigNumber = uIntNFromBigNumber s256 >>> map SignalId