module Etherium.TxOpts where

import Prelude

import Data.Lens ((?~))
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (TransactionOptions, _from, _gas, _to, defaultTransactionOptions)
import Network.Ethereum.Web3 as BN
import Network.Ethereum.Web3.Types (NoPay)

txOpts :: { to :: Address, from :: Address } -> TransactionOptions NoPay
txOpts { from, to } = defaultTransactionOptions
  # _from ?~ from
  # _to   ?~ to
  # _gas  ?~ BN.embed 9999999



txTo :: Address -> TransactionOptions NoPay
txTo to = defaultTransactionOptions
  # _to   ?~ to
  # _gas  ?~ BN.embed 9999999
