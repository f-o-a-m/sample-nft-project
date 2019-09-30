module App.Data.Signal where

import Prelude

import App.Data.Radius (Radius)
import App.Data.SaleId (SaleId)
import App.Data.SignalId (SignalId)
import App.Data.Token (Token)
import Control.Error.Util (hush)
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Geohash (Geohash)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Network.Ethereum.Core.Signatures (Address)
import Record.Extra (sequenceRecord)

newtype Signal = Signal
  { id :: SignalId
  , stake :: Token
  , owner :: Address
  , geohash :: Geohash
  , radius :: Radius
  , sale :: Maybe { id :: SaleId, price :: Token }
  }


derive instance newtypeSignal :: Newtype Signal _
derive instance genericSignal :: Generic Signal _
instance eqSignal :: Eq Signal where eq = genericEq
instance ordSignal :: Ord Signal where compare = genericCompare
instance showSignal :: Show Signal where show = genericShow

instance decodeJsonSignal :: DecodeJson Signal where
  decodeJson = decodeJson >=> \obj -> Signal <$> sequenceRecord
    { id: obj .: "id"
    , stake: obj .: "stake"
    , owner: obj .: "owner"
    , geohash: obj .: "geohash"
    , radius: obj .: "radius"
    , sale: pure $ hush $ sequenceRecord
        { id: obj .: "saleId"
        , price: obj .: "price"
        }
    }

signalId :: Signal -> SignalId
signalId (Signal {id}) = id
