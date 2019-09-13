{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.Models.SignalMarketSignalForSale where

import           Opaleye (Field, Table, table, tableField, SqlNumeric)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           SignalMarket.Common.EventTypes (TokenID, Value)

-- SignalMarket
-- SignalForSale :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalForSale' tokenID price = SignalForSale
  { tokenID :: tokenID
  , price :: price
  }

$(makeAdaptorAndInstance "pSignalForSale" ''SignalForSale')

type SignalForSalePG = SignalForSale' (Field SqlNumeric) (Field SqlNumeric)
type SignalForSale = SignalForSale' TokenID Value

signalForSaleTable :: Table SignalForSalePG SignalForSalePG
signalForSaleTable = table "signalForSale"
                           (pSignalForSale SignalForSale { tokenID = tableField "tokenID"
                                                         , price = tableField "price"
                                                         }
                           )
