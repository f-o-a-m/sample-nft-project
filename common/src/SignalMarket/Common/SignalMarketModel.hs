{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SignalMarket.Common.SignalMarketModel where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
-- import Data.Solidity.Prim.Address (Address)
import           Opaleye (Field, Select, selectTable, runSelect, runInsertMany, constant,
                         Table, table, tableField, SqlText, SqlNumeric, ToFields)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Profunctor.Product.Default
import           Data.Scientific (Scientific)
import           Opaleye.RunQuery (QueryRunnerColumnDefault)
import           Data.Int (Int64)


-- currently not being used
newtype EventId = EventId Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

newtype TokenId = TokenId Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)

newtype EthAddress = EthAddress Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

--------------------------------------------------------------------------------
{-
SignalMarket
- SignalSold :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6)),owner :: Address,newOwner :: Address}
- SignalForSale :: {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6))}
- SignalUnlisted :: {signalId :: (UIntN (D2 :& D5 :& DOne D6))}

-}

data SignalSold' signalSoldSignalId signalSoldPrice owner newOwner = SignalSold
  { signalSoldSignalId :: signalSoldSignalId
  , signalSoldPrice :: signalSoldPrice
  , owner :: owner
  , newOwner :: newOwner
  }

$(makeAdaptorAndInstance "pSignalSold" ''SignalSold')

type SignalSoldPG = SignalSold' (Field SqlNumeric) (Field SqlNumeric) (Field Text) (Field Text)
type SignalSold = SignalSold' Scientific Scientific Text Text

signalSoldTable :: Table SignalSoldPG SignalSoldPG
signalSoldTable = table "signalSold"
                        (pSignalSold SignalSold { signalSoldSignalId = tableField "signalSold"
                                                , signalSoldPrice = tableField "price"
                                                , owner = tableField "owner"
                                                , newOwner = tableField "newOwner"
                                                }
                        )

data SignalForSale' signalForSaleSignalId signalForSalePrice = SignalForSale
  { signalForSaleSignalId :: signalForSaleSignalId
  , signalForSalePrice :: signalForSalePrice
  }

$(makeAdaptorAndInstance "pSignalForSale" ''SignalForSale')

type SignalForSalePG = SignalForSale' (Field SqlNumeric) (Field SqlNumeric)
type SignalForSale = SignalForSale' Scientific Scientific

signalForSaleTable :: Table SignalForSalePG SignalForSalePG
signalForSaleTable = table "signalForSale"
                           (pSignalForSale SignalForSale { signalForSaleSignalId = tableField "signalSold"
                                                         , signalForSalePrice = tableField "price"
                                                         }
                           )

data SignalUnlisted' signalUnlistedSignalId = SignalUnlisted
  { signalUnlistedSignalId :: signalUnlistedSignalId
  }

$(makeAdaptorAndInstance "pSignalUnlisted" ''SignalUnlisted')

type SignalUnlistedPG = SignalUnlisted' (Field SqlNumeric)
type SignalUnlisted = SignalUnlisted' Scientific

signalUnlistedTable :: Table SignalUnlistedPG SignalUnlistedPG
signalUnlistedTable = table "signalUnlisted"
                            (pSignalUnlisted SignalUnlisted { signalUnlistedSignalId = tableField "signalSold"
                                                            }
                            )

--------------------------------------------------------------------------------

inserter
  :: Default ToFields haskells fields
  => Connection
  -> Table fields fields
  -> haskells
  -> IO Int64
inserter conn table a =
 runInsertMany conn table [constant a]
