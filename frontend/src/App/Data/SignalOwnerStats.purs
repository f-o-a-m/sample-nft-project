module App.Data.SignalOwnerStats where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.Signatures (Address)
import Record.Extra (sequenceRecord)

newtype SignalOwnerStats = SignalOwnerStats
    { ethAddress            :: Address
    , numberOwned           :: BigNumber
    , numberOfPurchases     :: BigNumber
    , averagePurchasePrice  :: BigNumber
    , totalPurchases        :: BigNumber
    , numberOfSales         :: BigNumber
    , averageSalePrice      :: BigNumber
    , totalSales            :: BigNumber
    }
derive instance genericSignalOwnerStats :: Generic SignalOwnerStats _
instance eqSignalOwnerStats :: Eq SignalOwnerStats where eq = genericEq
instance showSignalOwnerStats :: Show SignalOwnerStats where show = genericShow
instance decodeJsonSignalOwnerStats :: DecodeJson SignalOwnerStats where
  decodeJson = decodeJson >=> \obj -> SignalOwnerStats <$> sequenceRecord
    { ethAddress: obj .: "ethAddress"
    , numberOwned: obj .: "numberOwned"
    , numberOfPurchases: obj .: "numberOfPurchases"
    , averagePurchasePrice: obj .: "averagePurchasePrice"
    , totalPurchases: obj .: "totalPurchases"
    , numberOfSales: obj .: "numberOfSales"
    , averageSalePrice: obj .: "averageSalePrice"
    , totalSales: obj .: "totalSales"
    }
