{-# OPTIONS_GHC -fno-warn-orphans #-}

module SignalMarket.Server.API.Types where

import           Control.Error                        (fmapL)
import qualified Data.Aeson                           as A
import           Data.String.Conversions              (cs)
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

data Cursor = Cursor
  { limit  :: Int
  , offset :: Int
  }
