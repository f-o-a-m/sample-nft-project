module  Etherium.Tx where


import Prelude

import App.Data.ProviderState (ConnectedState)
import App.Data.SaleId (SaleId(..))
import App.Data.SignalId (SignalId(..))
import App.Data.Token (Token(..))
import App.MarketClient.Types (Contracts(..), ContractsR)
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Milliseconds(..), delay, fiberCanceler, launchAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Etherium.TxOpts (txOpts, txOpts')
import Network.Ethereum.Web3 (Address, HexString, Provider, TransactionReceipt(..), TransactionStatus(..), Web3, Web3Error, fromMinorUnit, runWeb3, unUIntN)
import Network.Ethereum.Web3.Api (eth_getTransactionReceipt)
import Network.Ethereum.Web3.Types (ETHER)

data Tx
  = UnSell SaleId
  | Sell SignalId (Token ETHER)
  | Buy SaleId (Token ETHER)

type Progress =
  { current :: Status
  , finished :: Array HexString
  , total :: Int
  }

data Status
  = Submitting
  | SubmittingFailed Web3Error
  | MiningStart HexString
  | MiningFailed TransactionReceipt
  | MiningFinished HexString

derive instance genericStatus :: Generic Status _
instance showStatus :: Show Status where show = genericShow

send
  :: Tx
  -> ConnectedState
  -> (Progress -> Effect Unit)
  -> Effect Canceler
send (UnSell (SaleId _saleId)) = send' \userAddress {signalMarket} ->
  NonEmptyList.singleton $ SignalMarket.unlist (txOpts {from: userAddress, to: signalMarket}) { _saleId }
send (Sell (SignalId _tokenId) (Token _price)) = send' \userAddress {signalMarket, signalToken} ->
  NonEmptyList.cons
    (SignalToken.approve (txOpts {from: userAddress, to: signalToken}) {to: signalMarket, tokenId: _tokenId})
    (NonEmptyList.singleton $ SignalMarket.forSale (txOpts {from: userAddress, to: signalMarket}) { _tokenId, _price })
send (Buy (SaleId _saleId) (Token price)) = send' \userAddress {signalMarket, foamToken} ->
  NonEmptyList.singleton $ SignalMarket.buy (txOpts' {from: userAddress, to: signalMarket, value: fromMinorUnit $ unUIntN price}) { _saleId }

send'
  :: (Address -> ContractsR -> NonEmptyList (Web3 HexString))
  -> ConnectedState
  -> (Progress -> Effect Unit)
  -> Effect Canceler
send' process connection cb = fiberCanceler <$> launchAff do
  let
    computations = process connection.userAddress (un Contracts connection.contracts)
    total = NonEmptyList.length computations
    {head, tail} = NonEmptyList.uncons computations
  go {total, finished: []} head tail
  where
    go p computation computations = do
      let emit current = liftEffect $ cb {current, total: p.total,finished: p.finished}
      emit $ Submitting
      sendRes <- runWeb3 connection.provider computation
      case sendRes of
        Left err -> emit $ SubmittingFailed err
        Right txHash -> do
          emit $ MiningStart txHash
          awaitMined txHash connection.provider >>= case _ of
            Left err -> emit $ MiningFailed err
            Right _ -> case computations of
              List.Nil -> emit $ MiningFinished txHash
              List.Cons computation' computations' ->
                go (p{ finished = [txHash] <> p.finished }) computation' computations'


awaitMined :: HexString -> Provider -> Aff (Either TransactionReceipt Unit)
awaitMined txHash provider = do
  txReceipt@(TransactionReceipt {status}) <- fix \poll ->
    runWeb3 provider (eth_getTransactionReceipt txHash) >>= case _ of
      Left _ -> liftAff (delay $ Milliseconds 1000.0) *> poll
      Right txRec -> pure txRec
  case status of
    Failed -> pure $ Left txReceipt
    Succeeded -> pure $ Right unit
