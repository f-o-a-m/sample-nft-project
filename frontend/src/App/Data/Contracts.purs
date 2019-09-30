module App.Data.Contracts where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Network.Ethereum.Core.Signatures (Address)
import Record.Extra (sequenceRecord)

newtype Contracts = Contracts ContractsR

type ContractsR =
  { foamToken    :: Address
  , signalToken  :: Address
  , signalMarket :: Address
  , networkId    :: NetworkId
  }

derive instance newtypeContracts :: Newtype Contracts _
derive instance genericContracts :: Generic Contracts _
instance eqContracts :: Eq Contracts where eq = genericEq
instance showContracts :: Show Contracts where show = genericShow

instance decodeJsonContracts :: DecodeJson Contracts where
  decodeJson = decodeJson >=> \obj -> Contracts <$> sequenceRecord
    { networkId: obj .: "networkId"
    , foamToken: (obj .: "foamToken") >>= (_ .: "address")
    , signalToken: (obj .: "signalToken") >>= (_ .: "address")
    , signalMarket: (obj .: "signalMarket") >>= (_ .: "address")
    }

newtype NetworkId = NetworkId String

derive instance newtypeNetworkId :: Newtype NetworkId _
derive instance genericNetworkId :: Generic NetworkId _
instance eqNetworkId :: Eq NetworkId where eq = genericEq
instance showNetworkId :: Show NetworkId where show = genericShow
instance decodeJsonNetworkId :: DecodeJson NetworkId where
  decodeJson json = NetworkId <$> decodeJson json
