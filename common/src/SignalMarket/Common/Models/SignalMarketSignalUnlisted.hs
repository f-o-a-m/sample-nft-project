{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.Models.SignalMarketSignalUnlisted where

import           Opaleye (Field, Table, table, tableField, SqlText, SqlNumeric)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           SignalMarket.Common.EventTypes (TokenID)

-- SignalMarket
-- SignalUnlisted :: {signalId :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalUnlisted' tokenID = SignalUnlisted
  { tokenID :: tokenID
  }

$(makeAdaptorAndInstance "pSignalUnlisted" ''SignalUnlisted')

type SignalUnlistedPG = SignalUnlisted' (Field SqlNumeric)
type SignalUnlisted = SignalUnlisted' TokenID

signalUnlistedTable :: Table SignalUnlistedPG SignalUnlistedPG
signalUnlistedTable = table "signalUnlisted"
                            (pSignalUnlisted SignalUnlisted { tokenID = tableField "tokenID"
                                                            }
                            )
