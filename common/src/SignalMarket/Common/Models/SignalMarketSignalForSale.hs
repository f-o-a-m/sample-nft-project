{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalForSale where

import           Composite.Opaleye.TH           (deriveOpaleyeEnum)
import           Control.Applicative            (pure)
import           Data.Char                      (toLower)
import           Data.List                      (stripPrefix)
import           Data.Text                      (Text)

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EventID, TokenID, Value)

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

$(deriveOpaleyeEnum ''HStatus "status" (stripPrefix "h" . map toLower))

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
