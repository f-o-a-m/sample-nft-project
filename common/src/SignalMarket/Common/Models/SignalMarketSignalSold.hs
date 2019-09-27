{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalSold where

import qualified Data.Aeson                     as A
import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           GHC.Generics                   (Generic)
import qualified Katip                          as K
import           Opaleye                        (Field, SqlNumeric, SqlText,
                                                 Table, table, tableField)
import           SignalMarket.Common.Aeson      (defaultAesonOptions)
import           SignalMarket.Common.EventTypes (EthAddress, EventID, SaleID,
                                                 TokenID, Value)

-- SignalMarket
-- SignalSold :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6)),owner :: Address,newOwner :: Address}

data SignalSold' saleID tokenID price soldFrom soldTo eventID = SignalSold
  { saleID   :: saleID
  , tokenID  :: tokenID
  , price    :: price
  , soldFrom :: soldFrom
  , soldTo   :: soldTo
  , eventID  :: eventID
  } deriving Generic

$(makeAdaptorAndInstance "pSignalSold" ''SignalSold')

type SignalSoldPG = SignalSold' (Field SqlNumeric) (Field SqlNumeric) (Field SqlNumeric) (Field SqlText) (Field SqlText) (Field SqlText)
type SignalSold = SignalSold' SaleID TokenID Value EthAddress EthAddress EventID

signalSoldTable :: Table SignalSoldPG SignalSoldPG
signalSoldTable = table "signal_market_signal_sold"
                        (pSignalSold SignalSold { saleID = tableField "sale_id"
                                                , tokenID = tableField "token_id"
                                                , price = tableField "price"
                                                , soldFrom = tableField "sold_from"
                                                , soldTo = tableField "sold_to"
                                                , eventID = tableField "event_id"
                                                }
                        )

instance A.ToJSON SignalSold where
  toJSON = A.genericToJSON (defaultAesonOptions "")

instance A.FromJSON SignalSold where
  parseJSON = A.genericParseJSON (defaultAesonOptions "")

instance K.ToObject SignalSold

instance K.LogItem SignalSold where
  payloadKeys _ _ = K.AllKeys
