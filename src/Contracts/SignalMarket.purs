--------------------------------------------------------------------------------
-- | SignalMarket
--------------------------------------------------------------------------------

module Contracts.SignalMarket where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (ByteString, D2, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3, Tuple4(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
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
-- | SignalForSale
--------------------------------------------------------------------------------


newtype SignalForSale = SignalForSale {signalId :: (UIntN (D2 :& D5 :& DOne D6)),price :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSignalForSale :: Newtype SignalForSale _

instance eventFilterSignalForSale :: EventFilter SignalForSale where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "3f703ec6164447ab69dade32a14d88d3d0f02b71dcf5a037a126207bc57f1482")]

instance indexedEventSignalForSale :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "signalId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "price") (UIntN (D2 :& D5 :& DOne D6)))) SignalForSale where
  isAnonymous _ = false

derive instance genericSignalForSale :: Generic SignalForSale _

instance eventGenericSignalForSaleShow :: Show SignalForSale where
	show = genericShow

instance eventGenericSignalForSaleeq :: Eq SignalForSale where
	eq = genericEq

--------------------------------------------------------------------------------
-- | FoamTokenFn
--------------------------------------------------------------------------------


type FoamTokenFn = Tagged (SProxy "foamToken()") (Tuple0 )

foamToken :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
foamToken x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: FoamTokenFn)

--------------------------------------------------------------------------------
-- | ForSaleFn
--------------------------------------------------------------------------------


type ForSaleFn = Tagged (SProxy "forSale(uint256,uint256)") (Tuple2 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_price") (UIntN (D2 :& D5 :& DOne D6))))

forSale :: TransactionOptions NoPay -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _price :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
forSale x0 r = uncurryFields  r $ forSale' x0
   where
    forSale' :: TransactionOptions NoPay -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_price") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    forSale' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: ForSaleFn)

--------------------------------------------------------------------------------
-- | OnERC721ReceivedFn
--------------------------------------------------------------------------------


type OnERC721ReceivedFn = Tagged (SProxy "onERC721Received(address,address,uint256,bytes)") (Tuple4 Address Address (UIntN (D2 :& D5 :& DOne D6)) ByteString)

onERC721Received :: TransactionOptions NoPay -> Address -> Address -> (UIntN (D2 :& D5 :& DOne D6)) -> ByteString -> Web3 HexString
onERC721Received x0 x1 x2 x3 x4 = sendTx x0 ((tagged $ Tuple4 x1 x2 x3 x4) :: OnERC721ReceivedFn)

--------------------------------------------------------------------------------
-- | SignalToSaleFn
--------------------------------------------------------------------------------


type SignalToSaleFn = Tagged (SProxy "signalToSale(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

signalToSale :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple3 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Address))
signalToSale x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: SignalToSaleFn)

--------------------------------------------------------------------------------
-- | SignalTokenFn
--------------------------------------------------------------------------------


type SignalTokenFn = Tagged (SProxy "signalToken()") (Tuple0 )

signalToken :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
signalToken x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SignalTokenFn)