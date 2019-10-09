module App.Data.SignalOwnerStats where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Network.Ethereum.Core.Signatures (Address)

newtype SignalOwnerStats = SignalOwnerStats
    { ethAddress            :: Address
    , numberOwned           :: String
    , numberOfPurchases     :: String
    , averagePurchasePrice  :: String
    , totalPurchases        :: String
    , numberOfSales         :: String
    , averageSalePrice      :: String
    , totalSales            :: String
    }
derive instance genericSignalOwnerStats :: Generic SignalOwnerStats _
instance eqSignalOwnerStats :: Eq SignalOwnerStats where eq = genericEq
instance showSignalOwnerStats :: Show SignalOwnerStats where show = genericShow
instance decodeJsonSignalOwnerStats :: DecodeJson SignalOwnerStats where
  decodeJson o = SignalOwnerStats <$> decodeJson o
