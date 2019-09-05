--------------------------------------------------------------------------------
-- | SignalToken
--------------------------------------------------------------------------------

module Contracts.SignalToken where

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
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 (Tagged (SProxy "_token") Address))

constructor :: TransactionOptions NoPay -> HexString -> { _token :: Address } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_token") Address) -> Web3 HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 y2) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | TrackedToken
--------------------------------------------------------------------------------


newtype TrackedToken = TrackedToken {cst :: (BytesN (D3 :& DOne D2)),nftAddress :: Address,tokenID :: (UIntN (D2 :& D5 :& DOne D6)),geohash :: (BytesN (D3 :& DOne D2)),radius :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeTrackedToken :: Newtype TrackedToken _

instance eventFilterTrackedToken :: EventFilter TrackedToken where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ae066be719a025aa84ce2e5cb92f496010fb4a339d6d9a2aac332be5770d471a"),Nothing]

instance indexedEventTrackedToken :: IndexedEvent (Tuple1 (Tagged (SProxy "nftAddress") Address)) (Tuple4 (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "tokenID") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "geohash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "radius") (UIntN (D2 :& D5 :& DOne D6)))) TrackedToken where
  isAnonymous _ = false

derive instance genericTrackedToken :: Generic TrackedToken _

instance eventGenericTrackedTokenShow :: Show TrackedToken where
	show = genericShow

instance eventGenericTrackedTokeneq :: Eq TrackedToken where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {_from :: Address,_to :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),Nothing,Nothing,Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
	show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Approval
--------------------------------------------------------------------------------


newtype Approval = Approval {_owner :: Address,_approved :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeApproval :: Newtype Approval _

instance eventFilterApproval :: EventFilter Approval where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"),Nothing,Nothing,Nothing]

instance indexedEventApproval :: IndexedEvent (Tuple3 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_approved") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) Approval where
  isAnonymous _ = false

derive instance genericApproval :: Generic Approval _

instance eventGenericApprovalShow :: Show Approval where
	show = genericShow

instance eventGenericApprovaleq :: Eq Approval where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ApprovalForAll
--------------------------------------------------------------------------------


newtype ApprovalForAll = ApprovalForAll {_owner :: Address,_operator :: Address,_approved :: Boolean}

derive instance newtypeApprovalForAll :: Newtype ApprovalForAll _

instance eventFilterApprovalForAll :: EventFilter ApprovalForAll where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31"),Nothing,Nothing]

instance indexedEventApprovalForAll :: IndexedEvent (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_operator") Address)) (Tuple1 (Tagged (SProxy "_approved") Boolean)) ApprovalForAll where
  isAnonymous _ = false

derive instance genericApprovalForAll :: Generic ApprovalForAll _

instance eventGenericApprovalForAllShow :: Show ApprovalForAll where
	show = genericShow

instance eventGenericApprovalForAlleq :: Eq ApprovalForAll where
	eq = genericEq

--------------------------------------------------------------------------------
-- | OwnershipRenounced
--------------------------------------------------------------------------------


newtype OwnershipRenounced = OwnershipRenounced {previousOwner :: Address}

derive instance newtypeOwnershipRenounced :: Newtype OwnershipRenounced _

instance eventFilterOwnershipRenounced :: EventFilter OwnershipRenounced where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f8df31144d9c2f0f6b59d69b8b98abd5459d07f2742c4df920b25aae33c64820"),Nothing]

instance indexedEventOwnershipRenounced :: IndexedEvent (Tuple1 (Tagged (SProxy "previousOwner") Address)) (Tuple0 ) OwnershipRenounced where
  isAnonymous _ = false

derive instance genericOwnershipRenounced :: Generic OwnershipRenounced _

instance eventGenericOwnershipRenouncedShow :: Show OwnershipRenounced where
	show = genericShow

instance eventGenericOwnershipRenouncedeq :: Eq OwnershipRenounced where
	eq = genericEq

--------------------------------------------------------------------------------
-- | OwnershipTransferred
--------------------------------------------------------------------------------


newtype OwnershipTransferred = OwnershipTransferred {previousOwner :: Address,newOwner :: Address}

derive instance newtypeOwnershipTransferred :: Newtype OwnershipTransferred _

instance eventFilterOwnershipTransferred :: EventFilter OwnershipTransferred where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8be0079c531659141344cd1fd0a4f28419497f9722a3daafe3b4186f6b6457e0"),Nothing,Nothing]

instance indexedEventOwnershipTransferred :: IndexedEvent (Tuple2 (Tagged (SProxy "previousOwner") Address) (Tagged (SProxy "newOwner") Address)) (Tuple0 ) OwnershipTransferred where
  isAnonymous _ = false

derive instance genericOwnershipTransferred :: Generic OwnershipTransferred _

instance eventGenericOwnershipTransferredShow :: Show OwnershipTransferred where
	show = genericShow

instance eventGenericOwnershipTransferredeq :: Eq OwnershipTransferred where
	eq = genericEq

--------------------------------------------------------------------------------
-- | InterfaceId_ERC165Fn
--------------------------------------------------------------------------------


type InterfaceId_ERC165Fn = Tagged (SProxy "InterfaceId_ERC165()") (Tuple0 )

interfaceId_ERC165 :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (DOne D4)))
interfaceId_ERC165 x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: InterfaceId_ERC165Fn)

--------------------------------------------------------------------------------
-- | ApproveFn
--------------------------------------------------------------------------------


type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

approve :: TransactionOptions NoPay -> { _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
approve x0 r = uncurryFields  r $ approve' x0
   where
    approve' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    approve' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: ApproveFn)

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 (Tagged (SProxy "_owner") Address))

balanceOf :: TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x0 cm r = uncurryFields  r $ balanceOf' x0 cm
   where
    balanceOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_owner") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    balanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | BurnFn
--------------------------------------------------------------------------------


type BurnFn = Tagged (SProxy "burn(uint256)") (Tuple1 (Tagged (SProxy "tokenID") (UIntN (D2 :& D5 :& DOne D6))))

burn :: TransactionOptions NoPay -> { tokenID :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
burn x0 r = uncurryFields  r $ burn' x0
   where
    burn' :: TransactionOptions NoPay -> (Tagged (SProxy "tokenID") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    burn' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: BurnFn)

--------------------------------------------------------------------------------
-- | ComputeCSTFn
--------------------------------------------------------------------------------


type ComputeCSTFn = Tagged (SProxy "computeCST(address,uint256)") (Tuple2 (Tagged (SProxy "nftContract") Address) (Tagged (SProxy "tokenID") (UIntN (D2 :& D5 :& DOne D6))))

computeCST :: TransactionOptions NoPay -> ChainCursor -> { nftContract :: Address, tokenID :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
computeCST x0 cm r = uncurryFields  r $ computeCST' x0 cm
   where
    computeCST' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "nftContract") Address) -> (Tagged (SProxy "tokenID") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
    computeCST' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: ComputeCSTFn)

--------------------------------------------------------------------------------
-- | ControllerFn
--------------------------------------------------------------------------------


type ControllerFn = Tagged (SProxy "controller()") (Tuple0 )

controller :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
controller x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ControllerFn)

--------------------------------------------------------------------------------
-- | CstToIDFn
--------------------------------------------------------------------------------


type CstToIDFn = Tagged (SProxy "cstToID(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

cstToID :: TransactionOptions NoPay -> ChainCursor -> (BytesN (D3 :& DOne D2)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
cstToID x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: CstToIDFn)

--------------------------------------------------------------------------------
-- | ExistsFn
--------------------------------------------------------------------------------


type ExistsFn = Tagged (SProxy "exists(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

exists :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Boolean)
exists x0 cm r = uncurryFields  r $ exists' x0 cm
   where
    exists' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Boolean)
    exists' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: ExistsFn)

--------------------------------------------------------------------------------
-- | GetApprovedFn
--------------------------------------------------------------------------------


type GetApprovedFn = Tagged (SProxy "getApproved(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getApproved :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
getApproved x0 cm r = uncurryFields  r $ getApproved' x0 cm
   where
    getApproved' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    getApproved' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetApprovedFn)

--------------------------------------------------------------------------------
-- | GetCreatedOnFn
--------------------------------------------------------------------------------


type GetCreatedOnFn = Tagged (SProxy "getCreatedOn(bytes32)") (Tuple1 (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))))

getCreatedOn :: TransactionOptions NoPay -> ChainCursor -> { cst :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getCreatedOn x0 cm r = uncurryFields  r $ getCreatedOn' x0 cm
   where
    getCreatedOn' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getCreatedOn' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetCreatedOnFn)

--------------------------------------------------------------------------------
-- | GetDeletedOnFn
--------------------------------------------------------------------------------


type GetDeletedOnFn = Tagged (SProxy "getDeletedOn(bytes32)") (Tuple1 (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))))

getDeletedOn :: TransactionOptions NoPay -> ChainCursor -> { cst :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDeletedOn x0 cm r = uncurryFields  r $ getDeletedOn' x0 cm
   where
    getDeletedOn' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDeletedOn' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetDeletedOnFn)

--------------------------------------------------------------------------------
-- | GetGeohashFn
--------------------------------------------------------------------------------


type GetGeohashFn = Tagged (SProxy "getGeohash(bytes32)") (Tuple1 (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))))

getGeohash :: TransactionOptions NoPay -> ChainCursor -> { cst :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
getGeohash x0 cm r = uncurryFields  r $ getGeohash' x0 cm
   where
    getGeohash' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
    getGeohash' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetGeohashFn)

--------------------------------------------------------------------------------
-- | GetRadiusFn
--------------------------------------------------------------------------------


type GetRadiusFn = Tagged (SProxy "getRadius(bytes32)") (Tuple1 (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))))

getRadius :: TransactionOptions NoPay -> ChainCursor -> { cst :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getRadius x0 cm r = uncurryFields  r $ getRadius' x0 cm
   where
    getRadius' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getRadius' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetRadiusFn)

--------------------------------------------------------------------------------
-- | IsApprovedForAllFn
--------------------------------------------------------------------------------


type IsApprovedForAllFn = Tagged (SProxy "isApprovedForAll(address,address)") (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_operator") Address))

isApprovedForAll :: TransactionOptions NoPay -> ChainCursor -> { _owner :: Address, _operator :: Address } -> Web3 (Either CallError Boolean)
isApprovedForAll x0 cm r = uncurryFields  r $ isApprovedForAll' x0 cm
   where
    isApprovedForAll' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_owner") Address) -> (Tagged (SProxy "_operator") Address) -> Web3 (Either CallError Boolean)
    isApprovedForAll' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: IsApprovedForAllFn)

--------------------------------------------------------------------------------
-- | IsTrackedFn
--------------------------------------------------------------------------------


type IsTrackedFn = Tagged (SProxy "isTracked(bytes32)") (Tuple1 (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))))

isTracked :: TransactionOptions NoPay -> ChainCursor -> { cst :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError Boolean)
isTracked x0 cm r = uncurryFields  r $ isTracked' x0 cm
   where
    isTracked' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "cst") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError Boolean)
    isTracked' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: IsTrackedFn)

--------------------------------------------------------------------------------
-- | MintFn
--------------------------------------------------------------------------------


type MintFn = Tagged (SProxy "mint(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& DOne D6)))

mint :: TransactionOptions NoPay -> Address -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 HexString
mint x0 x1 x2 = sendTx x0 ((tagged $ Tuple2 x1 x2) :: MintFn)

--------------------------------------------------------------------------------
-- | MintSignalFn
--------------------------------------------------------------------------------


type MintSignalFn = Tagged (SProxy "mintSignal(address,uint256,bytes32,uint256)") (Tuple4 (Tagged (SProxy "owner") Address) (Tagged (SProxy "stake") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "geohash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "radius") (UIntN (D2 :& D5 :& DOne D6))))

mintSignal :: TransactionOptions NoPay -> { owner :: Address, stake :: (UIntN (D2 :& D5 :& DOne D6)), geohash :: (BytesN (D3 :& DOne D2)), radius :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
mintSignal x0 r = uncurryFields  r $ mintSignal' x0
   where
    mintSignal' :: TransactionOptions NoPay -> (Tagged (SProxy "owner") Address) -> (Tagged (SProxy "stake") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "geohash") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "radius") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    mintSignal' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 y1 y2 y3 y4) :: MintSignalFn)

--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | NumApplicationsFn
--------------------------------------------------------------------------------


type NumApplicationsFn = Tagged (SProxy "numApplications(address)") (Tuple1 (Tagged (SProxy "prover") Address))

numApplications :: TransactionOptions NoPay -> ChainCursor -> { prover :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
numApplications x0 cm r = uncurryFields  r $ numApplications' x0 cm
   where
    numApplications' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "prover") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    numApplications' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: NumApplicationsFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | OwnerOfFn
--------------------------------------------------------------------------------


type OwnerOfFn = Tagged (SProxy "ownerOf(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

ownerOf :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
ownerOf x0 cm r = uncurryFields  r $ ownerOf' x0 cm
   where
    ownerOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    ownerOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: OwnerOfFn)

--------------------------------------------------------------------------------
-- | RenounceOwnershipFn
--------------------------------------------------------------------------------


type RenounceOwnershipFn = Tagged (SProxy "renounceOwnership()") (Tuple0 )

renounceOwnership :: TransactionOptions NoPay -> Web3 HexString
renounceOwnership x0 = sendTx x0 ((tagged $ Tuple0 ) :: RenounceOwnershipFn)

--------------------------------------------------------------------------------
-- | SafeTransferFrom4Fn
--------------------------------------------------------------------------------


type SafeTransferFrom4Fn = Tagged (SProxy "safeTransferFrom4(address,address,uint256,bytes)") (Tuple4 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_data") ByteString))

safeTransferFrom4 :: TransactionOptions NoPay -> { _from :: Address, _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _data :: ByteString } -> Web3 HexString
safeTransferFrom4 x0 r = uncurryFields  r $ safeTransferFrom4' x0
   where
    safeTransferFrom4' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_data") ByteString) -> Web3 HexString
    safeTransferFrom4' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 y1 y2 y3 y4) :: SafeTransferFrom4Fn)

--------------------------------------------------------------------------------
-- | SafeTransferFrom3Fn
--------------------------------------------------------------------------------


type SafeTransferFrom3Fn = Tagged (SProxy "safeTransferFrom3(address,address,uint256)") (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

safeTransferFrom3 :: TransactionOptions NoPay -> { _from :: Address, _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
safeTransferFrom3 x0 r = uncurryFields  r $ safeTransferFrom3' x0
   where
    safeTransferFrom3' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    safeTransferFrom3' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SafeTransferFrom3Fn)

--------------------------------------------------------------------------------
-- | SetApprovalForAllFn
--------------------------------------------------------------------------------


type SetApprovalForAllFn = Tagged (SProxy "setApprovalForAll(address,bool)") (Tuple2 (Tagged (SProxy "_to") Address) (Tagged (SProxy "_approved") Boolean))

setApprovalForAll :: TransactionOptions NoPay -> { _to :: Address, _approved :: Boolean } -> Web3 HexString
setApprovalForAll x0 r = uncurryFields  r $ setApprovalForAll' x0
   where
    setApprovalForAll' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_approved") Boolean) -> Web3 HexString
    setApprovalForAll' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetApprovalForAllFn)

--------------------------------------------------------------------------------
-- | SetControllerFn
--------------------------------------------------------------------------------


type SetControllerFn = Tagged (SProxy "setController(address)") (Tuple1 (Tagged (SProxy "_controller") Address))

setController :: TransactionOptions NoPay -> { _controller :: Address } -> Web3 HexString
setController x0 r = uncurryFields  r $ setController' x0
   where
    setController' :: TransactionOptions NoPay -> (Tagged (SProxy "_controller") Address) -> Web3 HexString
    setController' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetControllerFn)

--------------------------------------------------------------------------------
-- | SupportsInterfaceFn
--------------------------------------------------------------------------------


type SupportsInterfaceFn = Tagged (SProxy "supportsInterface(bytes4)") (Tuple1 (Tagged (SProxy "_interfaceId") (BytesN (DOne D4))))

supportsInterface :: TransactionOptions NoPay -> ChainCursor -> { _interfaceId :: (BytesN (DOne D4)) } -> Web3 (Either CallError Boolean)
supportsInterface x0 cm r = uncurryFields  r $ supportsInterface' x0 cm
   where
    supportsInterface' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_interfaceId") (BytesN (DOne D4))) -> Web3 (Either CallError Boolean)
    supportsInterface' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: SupportsInterfaceFn)

--------------------------------------------------------------------------------
-- | SymbolFn
--------------------------------------------------------------------------------


type SymbolFn = Tagged (SProxy "symbol()") (Tuple0 )

symbol :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
symbol x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SymbolFn)

--------------------------------------------------------------------------------
-- | TokenBurntOnFn
--------------------------------------------------------------------------------


type TokenBurntOnFn = Tagged (SProxy "tokenBurntOn(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

tokenBurntOn :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
tokenBurntOn x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: TokenBurntOnFn)

--------------------------------------------------------------------------------
-- | TokenGeohashFn
--------------------------------------------------------------------------------


type TokenGeohashFn = Tagged (SProxy "tokenGeohash(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

tokenGeohash :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
tokenGeohash x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: TokenGeohashFn)

--------------------------------------------------------------------------------
-- | TokenMintedOnFn
--------------------------------------------------------------------------------


type TokenMintedOnFn = Tagged (SProxy "tokenMintedOn(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

tokenMintedOn :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
tokenMintedOn x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: TokenMintedOnFn)

--------------------------------------------------------------------------------
-- | TokenRadiusFn
--------------------------------------------------------------------------------


type TokenRadiusFn = Tagged (SProxy "tokenRadius(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

tokenRadius :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
tokenRadius x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: TokenRadiusFn)

--------------------------------------------------------------------------------
-- | TokenStakeFn
--------------------------------------------------------------------------------


type TokenStakeFn = Tagged (SProxy "tokenStake(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

tokenStake :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
tokenStake x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: TokenStakeFn)

--------------------------------------------------------------------------------
-- | TotalStakedFn
--------------------------------------------------------------------------------


type TotalStakedFn = Tagged (SProxy "totalStaked(address)") (Tuple1 (Tagged (SProxy "prover") Address))

totalStaked :: TransactionOptions NoPay -> ChainCursor -> { prover :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalStaked x0 cm r = uncurryFields  r $ totalStaked' x0 cm
   where
    totalStaked' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "prover") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    totalStaked' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: TotalStakedFn)

--------------------------------------------------------------------------------
-- | TransferFromFn
--------------------------------------------------------------------------------


type TransferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenID") (UIntN (D2 :& D5 :& DOne D6))))

transferFrom :: TransactionOptions NoPay -> { _from :: Address, _to :: Address, _tokenID :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transferFrom x0 r = uncurryFields  r $ transferFrom' x0
   where
    transferFrom' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenID") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transferFrom' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: TransferFromFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "_newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { _newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "_newOwner") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)