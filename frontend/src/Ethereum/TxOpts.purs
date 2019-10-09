module Etherium.TxOpts where

import Prelude

import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (TransactionOptions(..), Value, _from, _gas, _to, defaultTransactionOptions)
import Network.Ethereum.Web3 as BN
import Network.Ethereum.Web3.Types (ETHER, NoPay)

txOpts :: { to :: Address, from :: Address } -> TransactionOptions NoPay
txOpts { from, to } = defaultTransactionOptions
  # _from ?~ from
  # _to   ?~ to
  # _gas  ?~ BN.embed 500000

txOpts' :: forall u. { to :: Address, from :: Address, value :: Value (u ETHER) } -> TransactionOptions u
txOpts' { from, to, value } = TransactionOptions
  { from: Just from
  , to: Just to
  , value: Just value
  , gas: Just $ BN.embed 500000
  , gasPrice: Nothing
  , data: Nothing
  , nonce: Nothing
  }


txTo :: Address -> TransactionOptions NoPay
txTo to = defaultTransactionOptions
  # _to   ?~ to
  # _gas  ?~ BN.embed 500000
