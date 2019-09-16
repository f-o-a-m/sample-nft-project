{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalForSale where

import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Opaleye                         (Field, SqlNumeric, SqlText,
                                                  Table, table, tableField)
import           SignalMarket.Common.EventTypes  (EventID, TokenID, Value)

-- SignalMarket
-- SignalForSale :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalForSale' tokenID price eventID = SignalForSale
  { tokenID :: tokenID
  , price   :: price
  , eventID :: eventID
  }

$(makeAdaptorAndInstance "pSignalForSale" ''SignalForSale')

type SignalForSalePG = SignalForSale' (Field SqlNumeric) (Field SqlNumeric) (Field SqlText)
type SignalForSale = SignalForSale' TokenID Value EventID

signalForSaleTable :: Table SignalForSalePG SignalForSalePG
signalForSaleTable = table "signal_for_sale"
                           (pSignalForSale SignalForSale { tokenID = tableField "token_id"
                                                         , price = tableField "price"
                                                         , eventID = tableField "event_id"}
                           )
