module SignalMarket.Common.EventTypes where

import           Data.Text (Text)
import           Data.ByteString (ByteString)
import           Data.Scientific (Scientific)
import           Opaleye (SqlText, SqlNumeric, SqlBytea, ToFields, Column)
import           Opaleye.RunQuery (QueryRunnerColumnDefault, fieldQueryRunnerColumn)
import           Opaleye.Internal.RunQuery as IQ
import qualified Data.Profunctor.Product.Default as D
import           Data.Profunctor

newtype EventId = EventId Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

instance D.Default ToFields EventId (Column SqlText) where
  def = lmap (\(EventId a) -> a) D.def

newtype Value = Value Integer deriving (Eq, Show)

instance D.Default ToFields Value (Column SqlNumeric) where
  def = lmap (\(Value a) -> fromInteger @Scientific a) D.def

instance IQ.QueryRunnerColumnDefault SqlNumeric Value where
  defaultFromField = Value . toInteger . truncate . toRational <$> fieldQueryRunnerColumn @Scientific

newtype ByteNValue = ByteNValue ByteString deriving (Eq, Show, QueryRunnerColumnDefault SqlBytea)

instance D.Default ToFields ByteNValue (Column SqlBytea) where
  def = lmap (\(ByteNValue a) -> a) D.def

newtype TokenID = TokenID Integer deriving (Eq, Show)

instance D.Default ToFields TokenID (Column SqlNumeric) where
  def = lmap (\(TokenID a) -> fromInteger @Scientific a) D.def

instance IQ.QueryRunnerColumnDefault SqlNumeric TokenID where
  defaultFromField = TokenID . toInteger . truncate . toRational <$> fieldQueryRunnerColumn @Scientific

newtype EthAddress = EthAddress Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

instance D.Default ToFields EthAddress (Column SqlText) where
  def = lmap (\(EthAddress a) -> a) D.def
