module Test.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Network.Ethereum.Web3 (Address, Value)
import Servant.API (class EncodeQueryParam)
import Servant.API as API

-- BlockNumberOrdering
data BlockNumberOrdering = ASC | DESC

instance encodeQueryParamData :: API.EncodeQueryParam BlockNumberOrdering where
  encodeQueryParam ASC = show "asc"
  encodeQueryParam DESC = show "desc"

-- Transaction Receipt
newtype Receipt =
  Receipt { address :: Address
          -- @TODO: fill out the proper types for these
          , blockHash :: String
          , blockNumber :: String
          , transactionHash :: String
          }

derive instance genericReceipt :: Generic Receipt _
derive instance newtypeReceipt :: Newtype Receipt _
derive newtype instance eqReceipt :: Eq Receipt
instance showReceipt :: Show Receipt where show = genericShow

instance encodeReceipt :: Encode Receipt where
  encode = genericEncode defaultOptions

instance decodeReceipt :: Decode Receipt where
  decode = genericDecode defaultOptions

instance encodeJsonReceipt :: EncodeJson Receipt where
  encodeJson (Receipt photo) = encodeJson photo

instance decodeJsonReceipt :: DecodeJson Receipt where
  decodeJson x = Receipt <$> decodeJson x

-- config/contracts
newtype Contracts =
  Contracts { foamToken :: Receipt
            , signalToken :: Receipt
            , signalMarket :: Receipt
            , networkId :: String
            }

derive instance genericContracts :: Generic Contracts _
derive instance newtypeContracts :: Newtype Contracts _
derive newtype instance eqContracts :: Eq Contracts
instance showContracts :: Show Contracts where show = genericShow

instance encodeContracts :: Encode Contracts where
  encode = genericEncode defaultOptions

instance decodeContracts :: Decode Contracts where
  decode = genericDecode defaultOptions

instance encodeJsonContracts :: EncodeJson Contracts where
  encodeJson (Contracts photo) = encodeJson photo

instance decodeJsonContracts :: DecodeJson Contracts where
  decodeJson x = Contracts <$> decodeJson x

-- foam token transfer

newtype FoamTokenTransfer =
  FoamTokenTransfer { to :: Address
                    , from :: Address
                    }

derive instance genericFoamTokenTransfer :: Generic FoamTokenTransfer _
derive instance newtypeFoamTokenTransfer :: Newtype FoamTokenTransfer _
derive newtype instance eqFoamTokenTransfer :: Eq FoamTokenTransfer
instance showFoamTokenTransfer :: Show FoamTokenTransfer where show = genericShow

instance encodeFoamTokenTransfer :: Encode FoamTokenTransfer where
  encode = genericEncode defaultOptions

instance decodeFoamTokenTransfer :: Decode FoamTokenTransfer where
  decode = genericDecode defaultOptions

instance encodeJsonFoamTokenTransfer :: EncodeJson FoamTokenTransfer where
  encodeJson (FoamTokenTransfer photo) = encodeJson photo

instance decodeJsonFoamTokenTransfer :: DecodeJson FoamTokenTransfer where
  decodeJson x = FoamTokenTransfer <$> decodeJson x
