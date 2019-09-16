module SignalMarket.Indexer.Types where

import           Control.Lens                         (to, (^.))
import           Data.ByteArray                       (Bytes, convert, take)
import           Data.ByteArray.Encoding              (Base (Base16),
                                                       convertFromBase,
                                                       convertToBase)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8
import           Data.ByteArray.Sized                 (unSizedByteArray)
import           Data.Char                            (isHexDigit)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.Lazy                       as TL (toStrict)
import qualified Data.Text.Lazy.Builder               as B
import qualified Data.Text.Lazy.Builder.Int           as B

import           Crypto.Hash                          (Digest, hash)
import           Crypto.Hash.Algorithms               (Keccak_256)
import           Network.Ethereum.Api.Types           as W3
import           SignalMarket.Common.EventTypes       (EventID(..), HexString, parseHexString)
import           SignalMarket.Common.Models.RawChange as RC

mkEvent
  :: W3.Change
  -> e
  -> Event e
mkEvent foo@(W3.Change{..}) e = Event
  { eventEventID = undefined
  , eventRawEvent = RC.RawChange
    { logIndex = undefined
    , transactionHash = undefined
    , blockHash = undefined
    , address = undefined
    , eventID = undefined
    }
  , eventData = e
  } where eventID = deriveEID foo
  
data Event e = Event
  { eventEventID  :: EventID
  , eventRawEvent :: RC.RawChange
  , eventData     :: e
  }

deriveEID :: Change -> EventID
deriveEID Change{..} =
    let logIndexBs = case integerToBytes . unQuantity $ fromMaybe (error "log index from processed block") changeLogIndex of
          Left e   -> error $ "Failed to convert integer to bytestring." ++ e
          Right bs -> bs
        digest :: Digest Keccak_256 = hash (undefined -- (changeBlockHash ^. to (unSizedByteArray . fromMaybe (error "log index is always from processed block")))
                                            <> logIndexBs)
    in EventID . bytesToHexString . convert . C8.take 32 . convert $ digest
      -- the justifyRight is kinda important -- numbers arent represented as even-length hex strings
      -- but as 0xHEX_NUMBER. 64 hex digits = 32 bytes = biggest possible number in evm length
    where
      integerToBytes :: Integer -> Either String Bytes
      integerToBytes = convertFromBase Base16 . T.encodeUtf8 . T.justifyRight 64 '0' . TL.toStrict .  B.toLazyText . B.hexadecimal

bytesToHexString :: Bytes -> HexString
bytesToHexString bs = case parseHexString . T.decodeUtf8 . convertToBase Base16 $ bs of
  Left e  -> error $ "Error in bytesToHexString: " ++ e ++ " " ++ show bs
  Right a -> a
