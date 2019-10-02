module App.Data.SignalActivity where

import Prelude

import App.Data.SaleId (SaleId)
import App.Data.Token (Token)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Network.Ethereum.Core.Signatures (Address)

data SignalActivity
  = ListedForSale (ListedForSaleR ())
  | Soled (SoledR ())

derive instance genericSignalActivity :: Generic SignalActivity _
instance eqSignalId :: Eq SignalActivity where eq = genericEq
instance ordSignalId :: Ord SignalActivity where compare = genericCompare
instance showSignalId :: Show SignalActivity where show = genericShow


type ListedForSaleR r =
  { owner :: Address
  , saleID :: SaleId
  , timestamp :: DateTime
  , price :: Token
  | r
  }

type SoledR r =
  { owner :: Address
  , saleID :: SaleId
  , timestamp :: DateTime
  , buyer :: Address
  , price :: Token
  | r
  }