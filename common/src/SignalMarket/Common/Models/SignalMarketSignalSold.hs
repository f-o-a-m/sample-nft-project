{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.Models.SignalMarketSignalSold where

import           Data.Text (Text)
import           Opaleye (Field, Table, table, tableField, SqlText, SqlNumeric)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           Data.Scientific (Scientific)

-- SignalMarket
-- SignalSold :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6)),owner :: Address,newOwner :: Address}

data SignalSold' signalId price owner newOwner = SignalSold
  { signalId :: signalId
  , price :: price
  , owner :: owner
  , newOwner :: newOwner
  }

$(makeAdaptorAndInstance "pSignalSold" ''SignalSold')

type SignalSoldPG = SignalSold' (Field SqlNumeric) (Field SqlNumeric) (Field Text) (Field Text)
type SignalSold = SignalSold' Scientific Scientific Text Text

signalSoldTable :: Table SignalSoldPG SignalSoldPG
signalSoldTable = table "signalSold"
                        (pSignalSold SignalSold { signalId = tableField "signalSold"
                                                , price = tableField "price"
                                                , owner = tableField "owner"
                                                , newOwner = tableField "newOwner"
                                                }
                        )
