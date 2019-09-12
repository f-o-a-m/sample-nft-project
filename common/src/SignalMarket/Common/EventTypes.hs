module SignalMarket.Common.EventTypes where

import           Data.Text (Text)
import           Data.ByteString (ByteString)
import           Data.Scientific (Scientific)
import           Opaleye (SqlText, SqlNumeric, SqlBytea, ToFields, Column)
import           Opaleye.RunQuery (QueryRunnerColumnDefault, fieldQueryRunnerColumn)
import qualified Data.Profunctor.Product.Default as D
import           Data.Profunctor

newtype EventId = EventId Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

instance D.Default ToFields EventId (Column SqlText) where
  def =  lmap (\(EventId a) -> a) D.def

---
newtype Value = Value Integer deriving (Eq, Show)

instance D.Default ToFields Value (Column SqlNumeric) where
  def =  lmap (\(Value a) -> toScientific a) D.def
    where toScientific :: Integer -> Scientific
          toScientific = fromInteger

-- instance QueryRunnerColumnDefault SqlNumeric Value where
--   defaultFromField = Value . toInteger .  truncate . toRational <$> fieldQueryRunnerColumn-- @Scientific

newtype ByteNValue = ByteNValue ByteString deriving (Eq, Show, QueryRunnerColumnDefault SqlBytea)

instance D.Default ToFields ByteNValue (Column SqlBytea) where
  def =  lmap (\(ByteNValue a) -> a) D.def

newtype TokenID = TokenID Scientific deriving (Eq, Show, QueryRunnerColumnDefault SqlNumeric)

instance D.Default ToFields TokenID (Column SqlNumeric) where
  def =  lmap (\(TokenID a) -> a) D.def

newtype EthAddress = EthAddress Text deriving (Eq, Show, QueryRunnerColumnDefault SqlText)

instance D.Default ToFields EthAddress (Column SqlText) where
  def =  lmap (\(EthAddress a) -> a) D.def

---

-- instance D.Default ToFields HexInteger (Column SqlNumeric) where
--   def = Constant $ constant @Sci.Scientific . fromInteger . unHexInteger

-- instance QueryRunnerColumnDefault SqlNumeric HexInteger where
--   queryRunnerColumnDefault = HexInteger . truncate . toRational <$> fieldQueryRunnerColumn @Sci.Scientific  
