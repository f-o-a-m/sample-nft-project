{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.SignalMarketSignalUnlistedModel where

import           Opaleye (Field, Table, table, tableField, SqlText, SqlNumeric)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           Data.Scientific (Scientific)

-- SignalMarket
-- SignalUnlisted :: {signalId :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalUnlisted' signalId = SignalUnlisted
  { signalId :: signalId
  }

$(makeAdaptorAndInstance "pSignalUnlisted" ''SignalUnlisted')

type SignalUnlistedPG = SignalUnlisted' (Field SqlNumeric)
type SignalUnlisted = SignalUnlisted' Scientific

signalUnlistedTable :: Table SignalUnlistedPG SignalUnlistedPG
signalUnlistedTable = table "signalUnlisted"
                            (pSignalUnlisted SignalUnlisted { signalId = tableField "signalSold"
                                                            }
                            )
