{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalUnlisted where

import           Data.Profunctor.Product.TH     (makeAdaptorAndInstance)
import           Opaleye                        (Field, SqlNumeric, Table,
                                                 table, tableField)
import           SignalMarket.Common.EventTypes (SaleID)

-- SignalMarket
-- SignalUnlisted :: {signalId :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalUnlisted' saleID = SignalUnlisted
  { saleID :: saleID
  }

$(makeAdaptorAndInstance "pSignalUnlisted" ''SignalUnlisted')

type SignalUnlistedPG = SignalUnlisted' (Field SqlNumeric)
type SignalUnlisted = SignalUnlisted' SaleID

signalUnlistedTable :: Table SignalUnlistedPG SignalUnlistedPG
signalUnlistedTable = table "signal_unlisted"
                            (pSignalUnlisted SignalUnlisted { saleID = tableField "sale_id"
                                                            }
                            )
