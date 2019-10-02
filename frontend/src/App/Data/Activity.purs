module App.Data.Activity where

import Prelude

import App.Data.Signal (Signal(..))
import App.Data.SignalActivity (ListedForSaleR, SoledR)
import App.Data.Token (Token)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (un)
import Network.Ethereum.Core.Signatures (Address)

data Activity
  = TokenTransfer
    { from :: Address
    , to :: Address
    , amount :: Token
    , timestamp :: DateTime
    }
  | SignalListedForSale (ListedForSaleR (signal :: Signal))
  | SignalSoled (SoledR (signal :: Signal))


derive instance genericActivity :: Generic Activity _
instance eqSignalId :: Eq Activity where eq = genericEq
instance ordSignalId :: Ord Activity where compare = genericCompare
instance showSignalId :: Show Activity where show = genericShow


getUserAddresses :: Activity -> Array Address
getUserAddresses = case _ of
  TokenTransfer a -> [a.from, a.to]
  SignalListedForSale a -> [a.owner, (un Signal a.signal).owner]
  SignalSoled a -> [a.owner, a.buyer, (un Signal a.signal).owner]