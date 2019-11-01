
module Faucet
  ( main
  ) where

import Prelude

import App.API (getContracts)
import App.MarketClient.Client (Contracts(..))
import Chanterelle.Internal.Utils (getPrimaryAccount, pollTransactionReceipt)
import Contracts.FoamToken as FoamToken
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Network.Ethereum.Web3 (Address, Provider, Value, Web3, _from, _to, _value, convert, defaultTransactionOptions, embed, formatValue, httpProvider, mkAddress, mkHexString, mkValue, runWeb3, toMinorUnit, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types.EtherUnit (Ether)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe (unsafeFromJust)

main :: Effect Unit
main = void <<< launchAff $ do
  (Contracts {foamToken}) <- getContracts
  faucetAddrEnv <- liftEffect $ lookupEnv "FAUCET_ADDRESS" >>= maybe (throw "Missing Environment Variable 'FAUCET_ADDRESS'") pure
  valueEnv <- liftEffect $ fromMaybe "" <$> lookupEnv "FAUCET_VALUE"
  provider <- liftEffect $ httpProvider "http://localhost:8545"
  from <- liftAff $ expectWeb3 "eth_getAccounts" provider getPrimaryAccount
  let receiver = mkAddress' faucetAddrEnv
      value = unsafeFromJust "invalid value" $ uIntNFromBigNumber s256 $ toMinorUnit (mkValue (embed (fromMaybe 200 (fromString valueEnv))) :: Value Ether)
  let ethVal = mkValue (embed 100) :: Value Ether
  tx <- liftAff $ runWeb3 provider do
    txE <- eth_sendTransaction (defaultTransactionOptions # _to ?~ receiver # _from ?~ from # _value ?~ convert ethVal)
    -- fromFoamBalance <- FoamToken.balanceOf (defaultTransactionOptions # _to ?~ foamToken) Latest {account: from}
    -- fromEthBalance <- eth_getBalance from Latest
    -- log $ show fromFoamBalance
    -- log $ show fromEthBalance
    txF <- FoamToken.transfer
      (defaultTransactionOptions # _to ?~ foamToken # _from ?~ from)
      { recipient: receiver
      , amount: value
      }
    pure
      { txF
      , txE
      }
  case tx of
    Left err ->
      log $ "Error to transfer  to " <> show receiver <> ". Error: " <> show err
    Right tx' -> do
      log $ "Sent " <> formatValue ethVal <> " ETHER to address " <> show receiver
      log $ "Tx: " <> show tx'.txE
      log $ "Sent " <> show value <> " FOAM to address " <> show receiver
      log $ "Tx: " <> show tx'.txF
      log $ "pollTransactionReceipt"
      txR <- pollTransactionReceipt tx'.txF provider
      log $ show txR



expectWeb3
  :: forall a
    . Show a
  => String
  -> Provider
  -> Web3 a
  -> Aff a
expectWeb3 dbgMsg provider action = do
  res <- runWeb3 provider action
  log $ dbgMsg <> ", result: " <> show res
  expectRight res

expectRight
  :: forall a b
   . Show a
  => Either a b
  -> Aff b
expectRight = case _ of
  Left l -> throwError $ error $ "Expected Right but got (Left " <> show l <>")"
  Right r -> pure r


mkAddress' :: String -> Address
mkAddress' addr = case mkAddress =<< mkHexString addr of
  Nothing -> unsafeCrashWith $ "Invalid Address: " <> addr
  Just addr' -> addr'

