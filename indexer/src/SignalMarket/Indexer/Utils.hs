module SignalMarket.Indexer.Utils where

import           Data.Int                        (Int64)
import           Data.Profunctor.Product.Default
import           Data.String                     (fromString)
import           Database.PostgreSQL.Simple      (Connection)
import           Katip                           as K
import           Opaleye                         (Table, ToFields, constant,
                                                  runInsertMany)
import           SignalMarket.Indexer.Class

insert
  :: Default ToFields haskells fields
  => MonadPG m
  => K.Katip m
  => K.KatipContext m
  => K.LogItem haskells
  => Table fields fields
  -> haskells
  -> m ()
insert table a = do
  n <- runDB $ \conn ->
    runInsertMany conn table [constant a]
  if n == 1
    then K.katipAddContext a $ logFM DebugS "Inserted"
    else K.logFM K.ErrorS $ fromString ("Inserted " <> show n <> " rows, expected 1.")
