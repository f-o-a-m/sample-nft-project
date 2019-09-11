module SignalMarket.Common.EventTypes where

import           Data.Text (Text)
import           Data.Scientific (Scientific)
import           Opaleye (SqlText, SqlNumeric, ToFields)
import           Opaleye.RunQuery (QueryRunnerColumnDefault)

newtype EventId = EventId Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)
newtype Value = Value Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)
newtype TokenID = TokenID Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)
newtype EthAddress = EthAddress Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)
