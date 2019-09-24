module SignalMarket.Common.EventTypes where

import           Control.Lens                         (Iso', from, iso, to,
                                                       view, (^.))
import qualified Data.Aeson                           as A
import qualified Data.ByteArray.HexString             as Hx
import           Data.ByteArray.Sized                 (unsafeSizedByteArray)
import           Data.Char                            (isHexDigit)
import           Data.Profunctor
import qualified Data.Profunctor.Product.Default      as D
import           Data.Scientific                      (Scientific)
import           Data.Solidity.Prim.Address           (Address, fromHexString,
                                                       toHexString)
import           Data.Solidity.Prim.Bytes             (BytesN)
import           Data.Solidity.Prim.Int               (UIntN)
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL (toStrict)
import qualified Data.Text.Lazy.Builder               as B
import qualified Data.Text.Lazy.Builder.Int           as B
import qualified Data.Text.Read                       as R
import qualified Database.PostgreSQL.Simple.FromField as FF
import           GHC.TypeLits
import           Opaleye                              (Column, Constant,
                                                       SqlNumeric, SqlText,
                                                       ToFields,
                                                       unsafeCoerceColumn)
import qualified Opaleye.Internal.RunQuery            as IQ
import           Opaleye.RunQuery                     (QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn)

--------------------------------------------------------------------------------
newtype HexInteger = HexInteger Integer deriving (Eq, Show, Ord)

hexIntegerToText :: HexInteger -> Text
hexIntegerToText (HexInteger n) = TL.toStrict . (<>) "0x" . B.toLazyText $ B.hexadecimal n

hexIntegerFromText :: Text -> Either String HexInteger
hexIntegerFromText t = case R.hexadecimal . maybeTrim $ t of
    Right (x, "") -> Right (HexInteger x)
    _             -> Left "Unable to parse HexInteger"
  where
    maybeTrim txt = if T.take 2 txt == "0x" then T.drop 2 txt else txt

instance A.ToJSON HexInteger where
    toJSON = A.toJSON . hexIntegerToText

instance A.FromJSON HexInteger where
    parseJSON (A.String v) = either fail pure . hexIntegerFromText $ v
    parseJSON _ = fail "HexInteger may only be parsed from a JSON String"

instance IQ.QueryRunnerColumnDefault SqlNumeric HexInteger where
  defaultFromField = HexInteger . toInteger . truncate . toRational <$> fieldQueryRunnerColumn @Scientific

instance D.Default ToFields HexInteger (Column SqlNumeric) where
  def = lmap (\(HexInteger a) -> fromInteger @Scientific a) D.def

_HexInteger :: (KnownNat n, n <= 256) => Iso' (UIntN n) HexInteger
_HexInteger = iso (HexInteger . toInteger) (\(HexInteger n) -> fromInteger n)

--------------------------------------------------------------------------------

newtype Value = Value HexInteger deriving (Eq, Show, A.ToJSON, A.FromJSON)

instance D.Default ToFields Value (Column SqlNumeric) where
  def = lmap (\(Value a) -> a) D.def

_Value :: (KnownNat n, n <= 256) => Iso' (UIntN n) Value
_Value = iso (view $ _HexInteger . to Value) (view $ to (\(Value v) -> v) . from _HexInteger)

instance IQ.QueryRunnerColumnDefault SqlNumeric Value where
  defaultFromField = Value . HexInteger . toInteger . truncate . toRational <$> fieldQueryRunnerColumn @Scientific

--------------------------------------------------------------------------------

newtype SaleID = SaleID HexInteger deriving (Eq, Show, IQ.QueryRunnerColumnDefault SqlNumeric, A.ToJSON, A.FromJSON)

_SaleID :: (KnownNat n, n <= 256) => Iso' (UIntN n) SaleID
_SaleID = iso (view $ _HexInteger . to SaleID) (view $ to (\(SaleID v) -> v) . from _HexInteger)

instance D.Default ToFields SaleID (Column SqlNumeric) where
  def = lmap (\(SaleID a) -> a) D.def

--------------------------------------------------------------------------------

newtype TokenID = TokenID HexInteger deriving (Eq, Show, IQ.QueryRunnerColumnDefault SqlNumeric, A.ToJSON, A.FromJSON)

_TokenID :: (KnownNat n, n <= 256) => Iso' (UIntN n) TokenID
_TokenID = iso (view $ _HexInteger . to TokenID) (view $ to (\(TokenID v) -> v) . from _HexInteger)

instance D.Default ToFields TokenID (Column SqlNumeric) where
  def = lmap (\(TokenID a) -> a) D.def

--------------------------------------------------------------------------------

newtype HexString = HexString Text
  deriving (Eq, Read, Show, Ord)

instance A.FromJSON HexString where
  parseJSON = A.withText "HexString" $ \txt ->
    either fail pure $ parseHexString txt

parseHexString :: Text -> Either String HexString
parseHexString = parse . T.toLower . trim0x
  where
    trim0x s  | T.take 2 s == "0x" = T.drop 2 s
              | otherwise = s
    parse txt | T.all isHexDigit txt && (T.length txt `mod` 2 == 0) = Right $ HexString txt
              | otherwise            = Left $ "Failed to parse text as HexString: " <> show txt

instance A.ToJSON HexString where
  toJSON (HexString txt) = A.String $ "0x" <> txt

instance QueryRunnerColumnDefault SqlText HexString where
  queryRunnerColumnDefault = fromRightWithError . parseHexString <$> fieldQueryRunnerColumn @Text
    where
      fromRightWithError eHex = case eHex of
        Left err  -> error $ "Error Parsing HexString from DB: " <> err
        Right res -> res

instance D.Default ToFields HexString (Column SqlText) where
  def = lmap (\(HexString a) -> a) D.def

_HexString :: Iso' Hx.HexString HexString
_HexString = iso (HexString . Hx.toText) (\(HexString hx) -> fromString $ T.unpack hx)


--------------------------------------------------------------------------------

newtype EventID = EventID HexString deriving (Eq, Show, QueryRunnerColumnDefault SqlText, A.ToJSON, A.FromJSON)

instance D.Default ToFields EventID (Column SqlText) where
  def = lmap (\(EventID a) -> a) D.def

--------------------------------------------------------------------------------

newtype ByteNValue = ByteNValue HexString deriving (Eq, Show, IQ.QueryRunnerColumnDefault SqlText, A.ToJSON, A.FromJSON)

instance D.Default ToFields ByteNValue (Column SqlText) where
  def = lmap (\(ByteNValue a) -> a) D.def

_HexBytesN :: (KnownNat n, n <= 32) => Iso' (BytesN n) ByteNValue
_HexBytesN = iso (ByteNValue . view _HexString . Hx.fromBytes) (\(ByteNValue bs) -> bs ^. from _HexString . to Hx.toBytes . to unsafeSizedByteArray)


--------------------------------------------------------------------------------

newtype EthAddress = EthAddress HexString deriving (Eq, Show, QueryRunnerColumnDefault SqlText, A.ToJSON, A.FromJSON)

instance D.Default ToFields EthAddress (Column SqlText) where
  def = lmap (\(EthAddress a) -> a) D.def

_EthAddress :: Iso' Address EthAddress
_EthAddress = iso (EthAddress . view _HexString . toHexString)
  (\(EthAddress a) -> either error id $ fromHexString $ view (from _HexString) a)

--------------------------------------------------------------------------------
-- the way we do enums is vendered from https://hackage.haskell.org/package/composite-opaleye-0.6.0.0/docs/Composite-Opaleye-TH.html#v:deriveOpaleyeEnum

data SaleStatus = SSActive | SSComplete | SSUnlisted

data SqlSaleStatus

instance FF.FromField SaleStatus where
  fromField f mbs = do
    tname <- FF.typename f
    case mbs of
      _ | tname /= "salestatus" -> FF.returnError FF.Incompatible f ""
      Just "active" -> pure SSActive
      Just "complete" -> pure SSComplete
      Just "unlisted" -> pure SSUnlisted
      Just other -> FF.returnError FF.ConversionFailed f ("Unexpected myenum value: " <> cs other)
      Nothing    -> FF.returnError FF.UnexpectedNull f ""

instance QueryRunnerColumnDefault SqlSaleStatus SaleStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant SaleStatus (Column SqlSaleStatus) where
  def = constantColumnUsing (D.def :: Constant String (Column SqlText)) $ \case
    SSActive -> "active"
    SSComplete -> "complete"
    SSUnlisted -> "unlisted"

instance A.FromJSON SaleStatus where
  parseJSON = A.withText "SaleStatus" $ \case
      "active" -> pure SSActive
      "complete" -> pure SSComplete
      "unlisted" -> pure SSUnlisted
      a -> fail "SaleStatus must be \"active\", \"complete\", or \"unlisted\"."

instance A.ToJSON SaleStatus where
  toJSON SSActive   = "active"
  toJSON SSComplete = "complete"
  toJSON SSUnlisted = "unlisted"

--------------------------------------------------------------------------------
-- vendored from https://hackage.haskell.org/package/composite-opaleye-0.6.0.0/docs/Composite-Opaleye-Util.html#v:constantColumnUsing
constantColumnUsing
  :: Constant haskell (Column pgType)
  -> (haskell' -> haskell)
  -> Constant haskell' (Column pgType')
constantColumnUsing oldConstant f = dimap f unsafeCoerceColumn oldConstant
