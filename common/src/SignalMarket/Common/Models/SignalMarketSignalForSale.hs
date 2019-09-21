{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalForSale where

import           Control.Applicative            (pure)
import qualified Data.Aeson                     as A
import           Data.Char                      (toLower)
import           Data.List                      (stripPrefix)
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EventID, SaleStatus,
                                                 SqlSaleStatus, TokenID, Value)

-- SignalMarket
-- SignalForSale :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalForSale' tokenID price saleStatus eventID = SignalForSale
  { tokenID    :: tokenID
  , price      :: price
  , saleStatus :: saleStatus
  , eventID    :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pSignalForSale" ''SignalForSale')

data HStatus = HActive | HComplete | HUnlisted deriving Generic

type SignalForSalePG = SignalForSale' (Field SqlNumeric) (Field SqlNumeric) (Field SqlSaleStatus) (Field SqlText)
type SignalForSale = SignalForSale' TokenID Value SaleStatus EventID

signalForSaleTable :: Table SignalForSalePG SignalForSalePG
signalForSaleTable = table "signal_for_sale"
                           (pSignalForSale SignalForSale { tokenID = tableField "token_id"
                                                         , price = tableField "price"
                                                         , saleStatus = tableField "sale_status"
                                                         , eventID = tableField "event_id"
                                                         }
                           )

instance A.ToJSON SignalForSale where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON SignalForSale where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject SignalForSale

instance K.LogItem SignalForSale where
  payloadKeys _ _ = K.AllKeys
