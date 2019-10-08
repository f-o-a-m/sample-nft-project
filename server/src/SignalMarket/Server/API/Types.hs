{-# OPTIONS_GHC -fno-warn-orphans #-}

module SignalMarket.Server.API.Types where

import           Control.Error                                         (fmapL)
import qualified Data.Aeson                                            as A
import           Data.String.Conversions                               (cs)
import           Data.Swagger                                          (SwaggerType (..),
                                                                        ToParamSchema (..),
                                                                        ToSchema (..),
                                                                        defaultSchemaOptions,
                                                                        genericDeclareNamedSchema)
import qualified Data.Text                                             as T
import           GHC.Generics                                          (Generic)
import qualified GHC.Generics                                          as GHC (Generic)
import           SignalMarket.Common.Aeson                             (defaultAesonOptions)
import           SignalMarket.Common.EventTypes                        (ByteNValue (..),
                                                                        EthAddress (..),
                                                                        HexInteger (..),
                                                                        HexString,
                                                                        SaleID (..),
                                                                        SaleStatus,
                                                                        TokenID (..),
                                                                        Value (..),
                                                                        hexIntegerFromText,
                                                                        parseHStatus,
                                                                        parseHexString)
import qualified SignalMarket.Common.Models.RawChange                  as RawChange
import qualified SignalMarket.Common.Models.SignalMarketSignalForSale  as ForSale
import qualified SignalMarket.Common.Models.SignalMarketSignalSold     as Sold
import qualified SignalMarket.Common.Models.SignalMarketSignalUnlisted as Unlisted
import qualified SignalMarket.Common.Models.SignalTokenTrackedToken    as TrackedToken
import           Web.HttpApiData                                       (FromHttpApiData (..))

type WithMetadataPG a = (a, RawChange.RawChangePG)

data WithMetadata a = WithMetadata
  { withMetadataData     :: a
  , withMetadataMetaData :: RawChange.RawChange
  } deriving Generic

instance (ToSchema a) => ToSchema (WithMetadata a) where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy

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

data BlockNumberOrdering = ASC | DESC deriving (Eq, Show, GHC.Generic)
instance ToParamSchema BlockNumberOrdering

instance FromHttpApiData BlockNumberOrdering where
    parseQueryParam qp
      | T.toLower qp == "asc" = Right ASC
      | T.toLower qp == "desc" = Right DESC
      | otherwise = Left "BlockNumberOrdering must be either \"asc\" or \"desc\""

data Cursor = Cursor
  { limit  :: Int
  , offset :: Int
  }

type SignalWithSaleResponse = [WithMetadata SignalWithSaleSummary]

data SignalWithSaleSummary = SignalWithSaleSummary
  { signalWithSaleResponseSignal :: TrackedToken.TrackedToken
  , signalWithSaleResponseSale   :: Maybe SaleSummary
  } deriving Generic

instance ToSchema SignalWithSaleSummary where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy


instance A.ToJSON SignalWithSaleSummary where
  toJSON = A.genericToJSON (defaultAesonOptions "signalWithSaleSummary")

data SaleSummary = SaleSummary
  { saleSummarySaleID :: SaleID
  , saleSummaryPrice  :: Value
  } deriving Generic

instance ToSchema SaleSummary where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy


instance A.ToJSON SaleSummary where
  toJSON = A.genericToJSON (defaultAesonOptions "saleSummary")

data SignalWithMarketHistoryResponse = SignalWithMarketHistoryResponse
  { signalWithMarketHistoryResponseSignal  :: WithMetadata TrackedToken.TrackedToken
  , signalWithMarketHistoryResponseHistory :: [WithMetadata MarketHistory]
  } deriving Generic

instance ToSchema SignalWithMarketHistoryResponse where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy


instance A.ToJSON SignalWithMarketHistoryResponse where
  toJSON = A.genericToJSON (defaultAesonOptions "signalWithMarketHistoryResponse")

data MarketHistory =
    ListedForSale ForSale.SignalForSale
  | Sold Sold.SignalSold
  | Unlisted Unlisted.SignalUnlisted
  deriving Generic

instance ToSchema MarketHistory where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy


instance A.ToJSON MarketHistory where
  toJSON = A.genericToJSON (defaultAesonOptions  "")
