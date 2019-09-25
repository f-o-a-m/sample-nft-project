{-# OPTIONS_GHC -fno-warn-orphans #-}

module SignalMarket.Server.API.Types where

import           Control.Error                        (fmapL)
import qualified Data.Aeson                           as A
import           Data.String.Conversions              (cs)
import qualified Data.Text                            as T
import           Data.Text.Read                       (decimal)
import           GHC.Generics                         (Generic)
import           SignalMarket.Common.Aeson            (defaultAesonOptions)
import           SignalMarket.Common.EventTypes       (EthAddress (..),
                                                       HexInteger (..),
                                                       HexString, SaleID (..),
                                                       SaleStatus, TokenID (..),
                                                       Value (..),
                                                       hexIntegerFromText,
                                                       parseHStatus,
                                                       parseHexString)
import           SignalMarket.Common.Models.RawChange as RawChange
import           Web.HttpApiData                      (FromHttpApiData (..))

data WithMetadata a = WithMetadata
  { withMetadataData     :: a
  , withMetadataMetaData :: RawChange.RawChange
  } deriving Generic

instance A.ToJSON a => A.ToJSON (WithMetadata a) where
  toJSON = A.genericToJSON (defaultAesonOptions "withMetadata")

instance FromHttpApiData HexString where
  parseQueryParam = fmapL cs . parseHexString

deriving instance FromHttpApiData EthAddress

instance FromHttpApiData Value where
  parseQueryParam x = do
    (val, remainder) <- fmapL cs (decimal x)
    if T.null remainder
      then Right (Value (HexInteger val))
      else Left "Malformed Number."

instance FromHttpApiData HexInteger where
  parseQueryParam = fmapL cs . hexIntegerFromText

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
