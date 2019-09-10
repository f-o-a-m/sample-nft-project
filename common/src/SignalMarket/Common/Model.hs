module SignalMarket.Common.Model where

import Data.ByteString (ByteString)
import Data.Solidity.Prim.Address (Address)

newtype EventId = EventId ByteString

newtype Value = Value Integer

newtype EthAddress = EthAddress Address

data Transfer' to from value = Transfer
  { to :: to
  , from :: from
  , value :: value
  , eid :: EventId
  }

type TransferPG = Transfer' ByteString ByteString Integer
type Transfer = Transfer' EthAddress EthAddress Value