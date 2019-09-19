{-# OPTIONS_GHC -fno-warn-orphans #-}

module SignalMarket.Server.API.Types where

import           Control.Error                        (fmapL)
import qualified Data.Aeson                           as A
import           Data.String.Conversions              (cs)
import qualified Data.Text                            as T
import           GHC.Generics                         (Generic)
import           SignalMarket.Common.Aeson            (defaultAesonOptions)
import           SignalMarket.Common.EventTypes       (EthAddress (..),
                                                       HexString,
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
