module Data.Geohash
  ( Geohash(..)
  , toGeohash
  , fromGeohash
  , geohashToHex
  , geohashToBS
  , geohashToLngLat
  , geohashFromHex
  , geohashFromBS
  , geohashFromLngLat
  , geohashToBS32
  ) where

import Prelude

import Data.ByteString as BS
import Data.Either (hush)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (hexadecimal, radix)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Exception (try)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Network.Ethereum.Core.BigNumber (parseBigNumber, toString)
import Network.Ethereum.Core.HexString (HexString, mkHexString, padLeft, toByteString, toHexString, unHex)
import Network.Ethereum.Web3 (BytesN, fromByteString)
import Network.Ethereum.Web3.Solidity.Sizes (S32, s32)
import Node.Encoding (Encoding(Hex))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

newtype Geohash = Geohash String

derive instance newtypeGeohash :: Newtype Geohash _
derive instance genericGeohash :: Generic Geohash _
instance eqGeohash :: Eq Geohash where eq = genericEq
instance ordGeohash :: Ord Geohash where compare = genericCompare
instance showGeohash :: Show Geohash where show = genericShow

foreign import toGeohash :: Fn2 Int {lat :: Number, lon :: Number} Geohash

foreign import fromGeohash :: EffectFn1 Geohash {lat :: Number, lon :: Number}

type LngLat = {lng :: Number, lat :: Number}

geohashToLngLat :: Geohash -> Maybe LngLat
geohashToLngLat gh =
  hush (unsafePerformEffect $ try $ runEffectFn1 fromGeohash gh) <#>
    \{lat, lon} -> {lng: lon, lat}

geohashFromLngLat :: Int -> LngLat -> Geohash
geohashFromLngLat precision lngLat =
  (runFn2 toGeohash) precision {lat: lngLat.lat, lon: lngLat.lng}

geohashToHex :: Geohash -> HexString
geohashToHex (Geohash gh) = unsafePartial $ fromJust do
  base32 <- radix 32
  toHexString <$> parseBigNumber base32 (geoToBase32Alph gh)

geohashToBS :: Geohash -> BS.ByteString
geohashToBS gh =
  let hx = unHex <<< geohashToHex $ gh
  in unsafePartial $ fromJust $ BS.fromString hx Hex

geohashFromHex :: HexString -> Geohash
geohashFromHex hx = unsafePartial $ fromJust do
  n <- parseBigNumber hexadecimal $ unHex hx
  base32 <- radix 32
  pure <<< Geohash <<< base32AlphToGeo $ toString base32 n

geohashFromBS :: BS.ByteString -> Geohash
geohashFromBS bs = geohashFromHex <<< unsafePartial fromJust <<< mkHexString $ BS.toString bs Hex

geohashToBS32 :: Geohash -> BytesN S32
geohashToBS32 gh =
  case fromByteString s32 $ toByteString $ padLeft $ geohashToHex gh of
    Nothing -> unsafeCrashWith $ "geohash should be valid `BytesN S32`, but it wasn't " <> show gh
    Just gh' -> gh'

base32AlphToGeo :: String -> String
base32AlphToGeo = fromCharArray <<< map base32ToGeoChar <<< toCharArray
  where
    base32ToGeoChar c = unsafePartial $ case c of
      '0' -> '0'
      '1' -> '1'
      '2' -> '2'
      '3' -> '3'
      '4' -> '4'
      '5' -> '5'
      '6' -> '6'
      '7' -> '7'
      '8' -> '8'
      '9' -> '9'
      'a' -> 'b'
      'b' -> 'c'
      'c' -> 'd'
      'd' -> 'e'
      'e' -> 'f'
      'f' -> 'g'
      'g' -> 'h'
      'h' -> 'j'
      'i' -> 'k'
      'j' -> 'm'
      'k' -> 'n'
      'l' -> 'p'
      'm' -> 'q'
      'n' -> 'r'
      'o' -> 's'
      'p' -> 't'
      'q' -> 'u'
      'r' -> 'v'
      's' -> 'w'
      't' -> 'x'
      'u' -> 'y'
      'v' -> 'z'

geoToBase32Alph :: String -> String
geoToBase32Alph = fromCharArray <<< map geoCharToBase32 <<< toCharArray
  where
    geoCharToBase32 c = unsafePartial $ case c of
      '0' -> '0'
      '1' -> '1'
      '2' -> '2'
      '3' -> '3'
      '4' -> '4'
      '5' -> '5'
      '6' -> '6'
      '7' -> '7'
      '8' -> '8'
      '9' -> '9'
      'b' -> 'a'
      'c' -> 'b'
      'd' -> 'c'
      'e' -> 'd'
      'f' -> 'e'
      'g' -> 'f'
      'h' -> 'g'
      'j' -> 'h'
      'k' -> 'i'
      'm' -> 'j'
      'n' -> 'k'
      'p' -> 'l'
      'q' -> 'm'
      'r' -> 'n'
      's' -> 'o'
      't' -> 'p'
      'u' -> 'q'
      'v' -> 'r'
      'w' -> 's'
      'x' -> 't'
      'y' -> 'u'
      'z' -> 'v'
