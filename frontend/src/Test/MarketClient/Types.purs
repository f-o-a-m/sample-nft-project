module Test.MarketClient.Types where

import Prelude

import Data.Argonaut ((.:))
import Data.Argonaut as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber)
import Record.Extra (sequenceRecord)
import Servant.API as API

newtype MetaData = MetaData
  { logIndex         :: BigNumber
  , transactionIndex :: BigNumber
  , transactionHash  :: HexString
  , removed          :: Boolean
  , blockHash        :: HexString
  , blockNumber      :: BlockNumber
  , address          :: Address
  , data             :: HexString
  , topics           :: Array HexString
  }

derive instance genericMetadata :: Generic MetaData _

instance decodeMetadata :: Decode MetaData where
  decode = genericDecode defaultOptions

instance decodeJsonMetadata :: A.DecodeJson MetaData where
  decodeJson x = MetaData <$> A.decodeJson x

newtype WithMetaData a = WithMetaData
  { "data"     :: a
  , metaData :: MetaData
  }

derive instance genericWithMetadata :: Generic (WithMetaData a) _

instance decodeWithMetadata :: Decode a => Decode (WithMetaData a) where
  decode = genericDecode defaultOptions


instance decodeJsonWithMetadata :: A.DecodeJson a => A.DecodeJson (WithMetaData a) where
  decodeJson x = do
    obj <- A.decodeJson x
    _data <- obj A..: "data"
    metaData <- obj A..: "metaData"
    pure $ WithMetaData {"data": _data, metaData}

-- BlockNumberOrdering
data BlockNumberOrdering = ASC | DESC

instance encodeBNOQueryParamData :: API.EncodeQueryParam BlockNumberOrdering where
  encodeQueryParam ASC = "asc"
  encodeQueryParam DESC = "desc"

-- Transaction Receipt
newtype Receipt =
  Receipt { address :: Address
          -- @TODO: fill out the proper types for these
          , blockHash :: String
          , blockNumber :: String
          , transactionHash :: String
          }

derive instance genericReceipt :: Generic Receipt _

instance decodeReceipt :: Decode Receipt where
  decode = genericDecode defaultOptions

instance decodeJsonReceipt :: A.DecodeJson Receipt where
  decodeJson x = Receipt <$> A.decodeJson x

-- config/contracts
newtype Contracts = Contracts ContractsR

type ContractsR =
  { foamToken    :: Address
  , signalToken  :: Address
  , signalMarket :: Address
  , networkId    :: NetworkId
  }-- { foamToken :: Receipt
  -- , signalToken :: Receipt
  -- , signalMarket :: Receipt
  -- , networkId :: String
  -- }

derive instance newtypeContracts :: Newtype Contracts _
derive instance genericContracts :: Generic Contracts _

instance decodeContracts :: Decode Contracts where
  decode = genericDecode defaultOptions

instance decodeJsonContracts :: A.DecodeJson Contracts where
  decodeJson = A.decodeJson >=> \obj -> Contracts <$> sequenceRecord
    { networkId: obj .: "networkId"
    , foamToken: (obj .: "foamToken") >>= (_ .: "address")
    , signalToken: (obj .: "signalToken") >>= (_ .: "address")
    , signalMarket: (obj .: "signalMarket") >>= (_ .: "address")
    }
-- instance decodeJsonContracts :: A.DecodeJson Contracts where
--   decodeJson x = Contracts <$> A.decodeJson x

newtype NetworkId = NetworkId String

derive instance newtypeNetworkId :: Newtype NetworkId _
derive instance genericNetworkId :: Generic NetworkId _
instance eqNetworkId :: Eq NetworkId where eq = genericEq
instance showNetworkId :: Show NetworkId where show = genericShow

instance decodeNetworkId :: Decode NetworkId where
  decode = genericDecode defaultOptions

instance decodeJsonNetworkId :: A.DecodeJson NetworkId where
  decodeJson json = NetworkId <$> A.decodeJson json

networkName :: NetworkId -> String
networkName (NetworkId n) = case n of
  "1"      -> "Main Ethereum Network"
  "4"      -> "Rinkeby Testnet"
  "420123" -> "Cliquebait Dreamnet"
  _        -> "Unsupported Network: " <> show n

-- foam token transfer
newtype FoamTokenTransfer =
  FoamTokenTransfer { to :: Address
                    , from :: Address
                    }

derive instance genericFoamTokenTransfer :: Generic FoamTokenTransfer _

instance decodeFoamTokenTransfer :: Decode FoamTokenTransfer where
  decode = genericDecode defaultOptions

instance decodeJsonFoamTokenTransfer :: A.DecodeJson FoamTokenTransfer where
  decodeJson x = FoamTokenTransfer <$> A.decodeJson x
