{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module SignalMarket.Common.Models.SignalMarketSignalSold where

import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text                       (Text)
import           Opaleye                         (Field, SqlNumeric, SqlText,
                                                  Table, table, tableField)
import           SignalMarket.Common.EventTypes  (EthAddress, TokenID, Value)

-- SignalMarket
-- SignalSold :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6)),owner :: Address,newOwner :: Address}

data SignalSold' tokenID price soldFrom soldTo = SignalSold
  { tokenID  :: tokenID
  , price    :: price
  , soldFrom :: soldFrom
  , soldTo   :: soldTo
  }

$(makeAdaptorAndInstance "pSignalSold" ''SignalSold')

type SignalSoldPG = SignalSold' (Field SqlNumeric) (Field SqlNumeric) (Field Text) (Field Text)
type SignalSold = SignalSold' TokenID Value EthAddress EthAddress

signalSoldTable :: Table SignalSoldPG SignalSoldPG
signalSoldTable = table "signal_sold"
                        (pSignalSold SignalSold { tokenID = tableField "token_id"
                                                , price = tableField "price"
                                                , soldFrom = tableField "sold_from"
                                                , soldTo = tableField "sold_to"
                                                }
                        )
