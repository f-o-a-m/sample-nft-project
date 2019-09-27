{-# LANGUAGE TemplateHaskell #-}

module SignalMarket.Common.Models.SignalMarketSignalForSale where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EthAddress, EventID, SaleID,
                                                 SaleStatus, SqlSaleStatus,
                                                 TokenID, Value)

data SignalForSale' saleID tokenID price saleStatus seller eventID = SignalForSale
  { saleID     :: saleID
  , tokenID    :: tokenID
  , price      :: price
  , saleStatus :: saleStatus
  , seller     :: seller
  , eventID    :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pSignalForSale" ''SignalForSale')

type SignalForSalePG = SignalForSale' (Field SqlNumeric) (Field SqlNumeric) (Field SqlNumeric) (Field SqlSaleStatus) (Field SqlText) (Field SqlText)
type SignalForSale = SignalForSale' SaleID TokenID Value SaleStatus EthAddress EventID

signalForSaleTable :: Table SignalForSalePG SignalForSalePG
signalForSaleTable = table "signal_market_signal_for_sale"
                           (pSignalForSale SignalForSale { saleID = tableField "sale_id"
                                                         , tokenID = tableField "token_id"
                                                         , price = tableField "price"
                                                         , saleStatus = tableField "sale_status"
                                                         , seller = tableField "seller"
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
