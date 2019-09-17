{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalUnlisted where

import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Opaleye                         (Field, SqlNumeric, SqlText,
                                                  Table, table, tableField)
import           SignalMarket.Common.EventTypes  (TokenID)

-- SignalMarket
-- SignalUnlisted :: {signalId :: (UIntN (D2 :& D5 :& DOne D6))}

data SignalUnlisted' tokenID = SignalUnlisted
  { tokenID :: tokenID
  }

$(makeAdaptorAndInstance "pSignalUnlisted" ''SignalUnlisted')

type SignalUnlistedPG = SignalUnlisted' (Field SqlNumeric)
type SignalUnlisted = SignalUnlisted' TokenID

signalUnlistedTable :: Table SignalUnlistedPG SignalUnlistedPG
signalUnlistedTable = table "signal_unlisted"
                            (pSignalUnlisted SignalUnlisted { tokenID = tableField "token_id"
                                                            }
                            )
