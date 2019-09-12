module SignalMarket.Common.EventTypes where

import           Data.Text (Text)
import           Data.Scientific (Scientific)
import           Opaleye (SqlText, SqlNumeric, ToFields, Column)
import           Opaleye.RunQuery (QueryRunnerColumnDefault)
import qualified Data.Profunctor.Product.Default as D
import Data.Profunctor
import GHC.Generics (Generic)


newtype EventId = EventId Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

instance D.Default ToFields EventId (Column SqlText) where
  def =  lmap (\(EventId a) -> a) D.def

newtype Value = Value Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)

instance D.Default ToFields Value (Column SqlNumeric) where
  def =  lmap (\(Value a) -> a) D.def

newtype TokenID = TokenID Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)

instance D.Default ToFields TokenID (Column SqlNumeric) where
  def =  lmap (\(TokenID a) -> a) D.def

newtype EthAddress = EthAddress Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

instance D.Default ToFields EthAddress (Column SqlText) where
  def =  lmap (\(EthAddress a) -> a) D.def
