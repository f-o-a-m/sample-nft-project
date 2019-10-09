module App.Data.SignalActivity where

import Prelude

import App.Data.SaleId (SaleId)
import App.Data.Token (Token)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3.Types (ETHER)

data SignalActivity
  = ListedForSale
      { owner :: Address
      , saleId :: SaleId
      , price :: Token ETHER
      }
  | UnlistedFromSale
      { owner :: Address
      , saleId :: SaleId
      }
  | Sold
      { owner :: Address
      , saleId :: SaleId
      , buyer :: Address
      , price :: Token ETHER
      }

derive instance genericSignalActivity :: Generic SignalActivity _
instance eqSignalId :: Eq SignalActivity where eq = genericEq
instance ordSignalId :: Ord SignalActivity where compare = genericCompare
instance showSignalId :: Show SignalActivity where show = genericShow
