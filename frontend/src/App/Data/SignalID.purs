module App.Data.SignalID
  ( SignalID
  , parse
  , toString
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

newtype SignalID = SignalID String

derive instance genericSignalID :: Generic SignalID _
derive instance eqSignalID :: Eq SignalID
derive instance ordSignalID :: Ord SignalID

derive newtype instance encodeJsonSignalID :: EncodeJson SignalID
derive newtype instance decodeJsonSignalID :: DecodeJson SignalID

instance showSignalID :: Show SignalID where show = genericShow

parse :: String -> Maybe SignalID
parse "" = Nothing
parse str = Just (SignalID str)

toString :: SignalID -> String
toString (SignalID str) = str