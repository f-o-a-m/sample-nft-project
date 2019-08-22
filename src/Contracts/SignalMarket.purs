--------------------------------------------------------------------------------
-- | SignalMarket
--------------------------------------------------------------------------------

module Contracts.SignalMarket where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, deployContract)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (Tuple0(..), Tuple2(..), unTuple1)
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,address)") (Tuple2 (Tagged (SProxy "_signalToken") Address) (Tagged (SProxy "_foamToken") Address))

constructor :: TransactionOptions NoPay -> HexString -> { _signalToken :: Address, _foamToken :: Address } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_signalToken") Address) -> (Tagged (SProxy "_foamToken") Address) -> Web3 HexString
    constructor' y0 bc' y2 y3 = deployContract y0 bc' ((tagged $ Tuple2 y2 y3) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | FoamTokenFn
--------------------------------------------------------------------------------


type FoamTokenFn = Tagged (SProxy "foamToken()") (Tuple0 )

foamToken :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
foamToken x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: FoamTokenFn)

--------------------------------------------------------------------------------
-- | SignalTokenFn
--------------------------------------------------------------------------------


type SignalTokenFn = Tagged (SProxy "signalToken()") (Tuple0 )

signalToken :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
signalToken x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SignalTokenFn)