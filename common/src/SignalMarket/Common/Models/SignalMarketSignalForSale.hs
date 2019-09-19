{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalForSale where


import           Data.Profunctor.Product.Default      (Default)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Opaleye.Internal.RunQuery            (QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn)

import           Control.Applicative                  (pure)
import           Data.Char                            (toLower)
import           Data.List                            (stripPrefix)
import           Data.Text                            (Text)

import qualified Data.Aeson                           as A
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           GHC.Generics                         (Generic)
import qualified Katip                                as K
import           Opaleye                              (Column, Constant, Field,
                                                       PGText, SqlNumeric,
                                                       SqlText, Table, table,
                                                       tableField)

import           SignalMarket.Common.Aeson            (defaultAesonOptions)
import           SignalMarket.Common.EventTypes       (EventID, TokenID, Value)

-- SignalMarket
-- SignalForSale :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalForSale' tokenID price eventID saleStatus = SignalForSale
  { tokenID    :: tokenID
  , price      :: price
  , eventID    :: eventID
  , saleStatus :: saleStatus
  } deriving Generic

$(makeAdaptorAndInstance "pSignalForSale" ''SignalForSale')

data HStatus = HActive | HComplete | HUnlisted deriving Generic

instance A.FromJSON HStatus where
  parseJSON (A.String s) =  pure $ mkHStatus s
  parseJSON _            = fail "Failed to parse HStatus object"

instance A.ToJSON HStatus where
  toJSON HActive   = "active"
  toJSON HComplete = "complete"
  toJSON HUnlisted = "unlisted"

mkHStatus :: Text -> HStatus
mkHStatus "active"    = HActive
mkHStatus "complete"  = HComplete
mkHStatus "unllisted" = HUnlisted
mkHStatus _           = error "Invalid status token"

-- $(deriveOpaleyeEnum ''HStatus "status" (stripPrefix "h" . map toLower))
data PGHStatus

instance FromField HStatus where
  fromField f mbs = do
    tname <- typename f
    case mbs of
      _ | tname /= "myenum" -> returnError Incompatible f ""
      Just "active" -> pure HActive
      Just "complete" -> pure HComplete
      Just "unlisted" -> pure HUnlisted
      Just other -> returnError ConversionFailed f ("Unexpected status value: " <> unpack other)
      Nothing    -> returnError UnexpectedNull f ""

instance QueryRunnerColumnDefault PGHStatus HStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant HStatus (Column PGHStatus) where
  def = constantColumnUsing (def :: Constant String (Column PGText)) $ \v ->
    case v of
      HActive   -> "active"
      HComplete -> "complete"
      HUnlisted -> "unlisted"

type SignalForSalePG = SignalForSale' (Field SqlNumeric) (Field SqlNumeric) (Field SqlText) (Field PGHStatus)
type SignalForSale = SignalForSale' TokenID Value EventID HStatus

signalForSaleTable :: Table SignalForSalePG SignalForSalePG
signalForSaleTable = table "signal_for_sale"
                           (pSignalForSale SignalForSale { tokenID = tableField "token_id"
                                                         , price = tableField "price"
                                                         , eventID = tableField "event_id"
                                                         , saleStatus = tableField "sale_status"
                                                         }
                           )

instance A.ToJSON SignalForSale where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON SignalForSale where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject SignalForSale

instance K.LogItem SignalForSale where
  payloadKeys _ _ = K.AllKeys
