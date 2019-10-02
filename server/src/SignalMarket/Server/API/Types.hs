{-# OPTIONS_GHC -fno-warn-orphans #-}

module SignalMarket.Server.API.Types where

import           Control.Error                        (fmapL)
import qualified Data.Aeson                           as A
import           Data.String.Conversions              (cs)
import qualified Data.Text                            as T
import           GHC.Generics                         (Generic)
import           SignalMarket.Common.Aeson            (defaultAesonOptions)
import           SignalMarket.Common.EventTypes       (ByteNValue (..),
                                                       EthAddress (..),
                                                       HexInteger (..),
                                                       HexString, SaleID (..),
                                                       SaleStatus, TokenID (..),
                                                       Value (..),
                                                       hexIntegerFromText,
                                                       parseHStatus,
                                                       parseHexString)
import           SignalMarket.Common.Models.RawChange as RawChange
import           Web.HttpApiData                      (FromHttpApiData (..))

type WithMetadataPG a = (a, RawChange.RawChangePG)

data WithMetadata a = WithMetadata
  { withMetadataData     :: a
  , withMetadataMetaData :: RawChange.RawChange
  } deriving Generic

instance A.ToJSON a => A.ToJSON (WithMetadata a) where
  toJSON = A.genericToJSON (defaultAesonOptions "withMetadata")

instance FromHttpApiData HexString where
  parseQueryParam = fmapL cs . parseHexString

deriving instance FromHttpApiData EthAddress
deriving instance FromHttpApiData ByteNValue

instance FromHttpApiData HexInteger where
  parseQueryParam = fmapL cs . hexIntegerFromText

deriving instance FromHttpApiData Value
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

data APISignal = APISignal
  { tokenID           :: TokenID     -- ^ NFT ID of the signal
  , owner             :: EthAddress  -- ^ current owner of the signal
  , cst               :: ByteNValue  -- ^ CST of the signal
  , geohash           :: ByteNValue  -- ^ Geohash of the signal's location
  , radius            :: HexInteger  -- ^ Radius of the signal
  , staked            :: Value       -- ^ FOAM Tokens staked within in the signal
  , isBurned          :: Bool
  , signalSaleSummary :: Maybe SaleSummary
  } deriving Generic

instance A.ToJSON APISignal where
  toJSON = A.genericToJSON (defaultAesonOptions "signal")

data SaleSummary = SaleSummary
  { saleSummarySaleID :: SaleID
  , saleSummaryPrice  :: Value
  } deriving Generic

instance A.ToJSON SaleSummary where
  toJSON = A.genericToJSON (defaultAesonOptions "saleSummary")
