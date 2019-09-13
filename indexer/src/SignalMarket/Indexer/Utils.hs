module SignalMarket.Indexer.Utils where

import Data.Int (Int64)
import           Data.Profunctor.Product.Default
import Opaleye (ToFields, Table, runInsertMany, constant)
import           Database.PostgreSQL.Simple         (Connection)

insert
  :: Default ToFields haskells fields
  => Connection
  -> Table fields fields
  -> haskells
  -> IO Int64
insert conn table a = runInsertMany conn table [constant a]