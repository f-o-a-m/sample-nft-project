--------------------------------------------------------------------------------
-- | TokenControllerMock
--------------------------------------------------------------------------------

module Contracts.TokenControllerMock where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (Tuple1(..), Tuple2(..), unTuple1)
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | PurchaseCheckFn
--------------------------------------------------------------------------------


type PurchaseCheckFn = Tagged (SProxy "purchaseCheck(address)") (Tuple1 (Tagged (SProxy "_contributor") Address))

purchaseCheck :: TransactionOptions NoPay -> ChainCursor -> { _contributor :: Address } -> Web3 (Either CallError Boolean)
purchaseCheck x0 cm r = uncurryFields  r $ purchaseCheck' x0 cm
   where
    purchaseCheck' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contributor") Address) -> Web3 (Either CallError Boolean)
    purchaseCheck' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: PurchaseCheckFn)

--------------------------------------------------------------------------------
-- | TransferAllowedFn
--------------------------------------------------------------------------------


type TransferAllowedFn = Tagged (SProxy "transferAllowed(address,address)") (Tuple2 Address Address)

transferAllowed :: TransactionOptions NoPay -> ChainCursor -> Address -> Address -> Web3 (Either CallError Boolean)
transferAllowed x0 cm x2 x3 = map unTuple1 <$> call x0 cm ((tagged $ Tuple2 x2 x3) :: TransferAllowedFn)