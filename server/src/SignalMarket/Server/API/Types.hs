{-# OPTIONS_GHC -fno-warn-orphans #-}

module SignalMarket.Server.API.Types where

import           Control.Error                                      (fmapL)
import qualified Data.Aeson                                         as A
import           Data.String.Conversions                            (cs)
import qualified Data.Text                                          as T
import           Data.Text.Read                                     (decimal)
import           GHC.Generics                                       (Generic)
import           SignalMarket.Common.Aeson                          (defaultAesonOptions)
import           SignalMarket.Common.EventTypes                     (EthAddress (..),
                                                                     HexInteger (..),
                                                                     HexString,
                                                                     SaleID (..),
                                                                     SaleStatus,
                                                                     TokenID (..),
                                                                     Value (..),
                                                                     hexIntegerFromText,
                                                                     parseHStatus,
                                                                     parseHexString)
import           SignalMarket.Common.Models.RawChange               as RawChange
import           SignalMarket.Common.Models.SignalTokenTrackedToken as SignalTokenTrackedToken
import           SignalMarket.Common.Models.SignalTokenTransfer     as SignalTokenTransfer
import           Web.HttpApiData                                    (FromHttpApiData (..))

data WithMetadata a = WithMetadata
  { withMetadataData     :: a
  , withMetadataMetaData :: RawChange.RawChange
  } deriving Generic

instance A.ToJSON a => A.ToJSON (WithMetadata a) where
  toJSON = A.genericToJSON (defaultAesonOptions "withMetadata")

instance FromHttpApiData HexString where
  parseQueryParam = fmapL cs . parseHexString

deriving instance FromHttpApiData EthAddress

instance FromHttpApiData HexInteger where
  parseQueryParam = fmapL cs . hexIntegerFromText

instance FromHttpApiData Value where
  parseQueryParam x = do
    (val :: Integer, remainder) <- fmapL cs (decimal x)
    if T.null remainder
      then Right (fromIntegral val)
      else Left "Malformed Number."

deriving instance FromHttpApiData SaleID
deriving instance FromHttpApiData TokenID

instance FromHttpApiData SaleStatus where
  parseQueryParam = fmapL cs . parseHStatus

data BlockNumberOrdering = ASC | DESC

instance FromHttpApiData BlockNumberOrdering where
    parseQueryParam qp
      | T.toLower qp == "asc" = Right ASC
      | T.toLower qp == "desc" = Right DESC
      | otherwise = Left "BlockNumberOrdering must be either \"asc\" or \"desc\""

data Cursor = Cursor
  { limit  :: Int
  , offset :: Int
  }

data ApiSignal = ApiSignal
  { token :: WithMetadata SignalTokenTrackedToken.TrackedToken -- ^ Event which created the token
  , owner :: WithMetadata SignalTokenTransfer.Transfer         -- ^ Event associated with the last ownership change (to address is current owner)
  } deriving Generic

instance A.ToJSON ApiSignal where
  toJSON = A.genericToJSON (defaultAesonOptions "signal")
