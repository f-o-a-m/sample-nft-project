module  Etherium.Tx where


import Prelude

import App.Data.Contracts (Contracts(..), ContractsR)
import App.Data.ProviderState (ConnectedState)
import App.Data.SaleId (SaleId(..))
import App.Data.SignalId (SignalId(..))
import App.Data.Token (Token(..))
import Contracts.SignalMarket as SignalMarket
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Canceler, Milliseconds(..), delay, fiberCanceler, launchAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Etherium.TxOpts (txOpts, txOpts')
import Network.Ethereum.Web3 (Address, HexString, TransactionReceipt(..), TransactionStatus(..), Value, Web3, Web3Error, runWeb3)
import Network.Ethereum.Web3.Api (eth_getTransactionReceipt)
import Network.Ethereum.Web3.Types (ETHER)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)

data Tx
  = UnSell SaleId
  | Sell SignalId Token
  | Buy SaleId (Value (MinorUnit ETHER))

data Status
  = Submitting
  | SubmittingFailed Web3Error
  | MiningStart HexString
  | MiningFailed TransactionReceipt
  | MiningFinished HexString

send
  :: Tx
  -> ConnectedState
  -> (Status -> Effect Unit)
  -> Effect Canceler
send (UnSell (SaleId _saleId)) = send'
  \userAddress {signalMarket} -> SignalMarket.unlist
    (txOpts {from: userAddress, to: signalMarket})
    { _saleId }
send (Sell (SignalId _tokenId) (Token _price)) = send'
  \userAddress {signalMarket} -> SignalMarket.forSale
    (txOpts {from: userAddress, to: signalMarket})
    { _tokenId, _price }
send (Buy (SaleId _saleId) price) = send'
  \userAddress {signalMarket} -> SignalMarket.buy
    (txOpts' {from: userAddress, to: signalMarket, value: price})
    { _saleId }

send'
  :: (Address -> ContractsR -> Web3 HexString)
  -> ConnectedState
  -> (Status -> Effect Unit)
  -> Effect Canceler
send' tx connection cb = fiberCanceler <$> launchAff do
  liftEffect $ cb $ Submitting
  sendRes <- runWeb3 connection.provider $ tx connection.userAddress (un Contracts connection.contracts)
  case sendRes of
    Left err -> liftEffect $ cb $ SubmittingFailed err
    Right txHash -> do
      liftEffect $ cb $ MiningStart txHash
      txReceipt@(TransactionReceipt {status}) <- fix \poll ->
        runWeb3 connection.provider (eth_getTransactionReceipt txHash) >>= case _ of
          Left _ -> liftAff (delay $ Milliseconds 3000.0) *> poll
          Right txRec -> pure txRec
      case status of
        Succeeded -> liftEffect $ cb $ MiningFinished txHash
        Failed -> liftEffect $ cb $ MiningFailed txReceipt

