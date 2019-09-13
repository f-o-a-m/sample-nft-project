{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.Models.SignalMarketSignalSold where

import           Data.Text (Text)
import           Opaleye (Field, Table, table, tableField, SqlText, SqlNumeric)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           SignalMarket.Common.EventTypes (TokenID, Value, EthAddress)

-- SignalMarket
-- SignalSold :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6)),owner :: Address,newOwner :: Address}

data SignalSold' tokenID price soldFrom soldTo = SignalSold
  { tokenID :: tokenID
  , price :: price
  , soldFrom :: soldFrom
  , soldTo :: soldTo
  }

$(makeAdaptorAndInstance "pSignalSold" ''SignalSold')

type SignalSoldPG = SignalSold' (Field SqlNumeric) (Field SqlNumeric) (Field Text) (Field Text)
type SignalSold = SignalSold' TokenID Value EthAddress EthAddress

signalSoldTable :: Table SignalSoldPG SignalSoldPG
signalSoldTable = table "signalSold"
                        (pSignalSold SignalSold { tokenID = tableField "tokenID"
                                                , price = tableField "price"
                                                , soldFrom = tableField "soldFrom"
                                                , soldTo = tableField "soldTo"
                                                }
                        )
