module SignalMarket.Common.EventTypes where

import           Data.ByteString                 (ByteString)
import           Data.Profunctor
import qualified Data.Profunctor.Product.Default as D
import           Data.Scientific                 (Scientific)
import           Data.Text                       (Text)
import           Opaleye                         (Column, SqlBytea, SqlNumeric,
                                                  SqlText, ToFields)
import           Opaleye.Internal.RunQuery       as IQ
import           Opaleye.RunQuery                (QueryRunnerColumnDefault,
                                                  fieldQueryRunnerColumn)

newtype EventID = EventID Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

instance D.Default ToFields EventID (Column SqlText) where
  def = lmap (\(EventID a) -> a) D.def

newtype HexInteger = HexInteger Integer deriving (Eq, Show)

instance D.Default ToFields HexInteger (Column SqlNumeric) where
  def = lmap (\(HexInteger a) -> fromInteger @Scientific a) D.def

newtype Value = Value HexInteger deriving (Eq, Show)

instance D.Default ToFields Value (Column SqlNumeric) where
  def = lmap (\(Value a) -> a) D.def

instance IQ.QueryRunnerColumnDefault SqlNumeric Value where
  defaultFromField = Value . HexInteger . toInteger . truncate . toRational <$> fieldQueryRunnerColumn @Scientific

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
