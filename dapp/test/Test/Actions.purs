module Test.Actions where

import Prelude

import Contracts.FoamToken as FoamToken
import Contracts.SignalMarket as SignalMarket
import Contracts.SignalToken as SignalToken
import Control.Parallel (parTraverse_)
import Data.Lens ((?~))
import Data.Newtype (unwrap)
import Deploy.Utils (awaitTxSuccess)
import Effect.Aff.Class (class MonadAff, liftAff)
import Network.Ethereum.Web3 (Address, BytesN, Ether, HexString, Provider, UIntN, Value, Web3, _from, _gas, _to, _value, convert, defaultTransactionOptions, embed, eventFilter, mkValue)
import Network.Ethereum.Web3.Api (eth_sendTransaction)
import Network.Ethereum.Web3.Solidity.Sizes (S32, S256, s256)
import Test.Utils (TestEnv, assertWeb3, mkUIntN, monitorUntil)
import Type.Proxy (Proxy(..))

faucet
  :: { recipient :: Address
     , foamToken :: Address
     , tokenFaucet :: Address
     }
  -> Web3 HexString
faucet { recipient, foamToken, tokenFaucet } =
  let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ tokenFaucet
  in FoamToken.transfer txOpts { recipient
                               , amount: mkUIntN s256 1000000
                               }

ethFaucetOne
  :: { recipient :: Address
     , tokenFaucet :: Address
     }
  -> Web3 HexString
ethFaucetOne { recipient, tokenFaucet } =
  let txOpts =
        defaultTransactionOptions # _to ?~ recipient
                                  # _value ?~ convert (mkValue one :: Value Ether)
                                  # _from ?~ tokenFaucet
  in eth_sendTransaction txOpts

faucetTokens
  :: forall m.
     MonadAff m
  => TestEnv m
  -> { account1 :: Address
     , account2 :: Address
     , foamToken :: Address
     , provider :: Provider
     , tokenFaucet :: Address
     }
  -> m Unit
faucetTokens { logger } { foamToken, tokenFaucet, provider, account1, account2 } = do
  -- give FOAM tokens to each of them (via faucet)
  liftAff $ flip parTraverse_ [account1, account2] \recipient -> do
    txHash <- assertWeb3 provider $ faucet { recipient, foamToken, tokenFaucet }
    awaitTxSuccess txHash provider
  logger $ "FAUCET: FOAM tokens"
  -- give one ETH to account2
  txHash <- assertWeb3 provider $ ethFaucetOne { recipient: account2
                                               , tokenFaucet
                                               }
  logger $ "FAUCET: ETH to account2 " <> show txHash
  awaitTxSuccess txHash provider

trackMintSignal
  :: forall m.
     MonadAff m
  => TestEnv m
  -> { account1 :: Address
     , geohash :: BytesN S32
     , radius :: UIntN S256
     , provider :: Provider
     , foamToken :: Address
     , signalToken :: Address
     }
  -> m { trackMint :: SignalToken.TrackedToken }
trackMintSignal { logger } { geohash, radius, foamToken, signalToken, provider, account1 } = do
  let txOpts = defaultTransactionOptions # _to ?~ foamToken
                                         # _from ?~ account1
                                         # _gas ?~ embed 8000000
      approvalAmount = mkUIntN s256 100
      approveAction = FoamToken.approve txOpts { spender: signalToken
                                               , value: approvalAmount
                                               }
      approvalFilter = eventFilter (Proxy :: Proxy FoamToken.Approval) foamToken
  approval@(FoamToken.Approval ft) <- monitorUntil provider approveAction approvalFilter
  logger $ "AUX: Approved FOAM tokens " <> show approval
  -- generate unique attributes
  let stake = mkUIntN s256 1
      owner = account1
      mintAction = SignalToken.mintSignal (txOpts # _to ?~ signalToken)
                                          { owner, stake, geohash, radius }
      trackedTokenFilter = eventFilter (Proxy :: Proxy SignalToken.TrackedToken) signalToken
  trackMint@(SignalToken.TrackedToken tm) <- monitorUntil provider mintAction trackedTokenFilter
  logger $ "AUX: Signal token minted " <> show trackMint
  pure { trackMint }

markSignalForSale
  :: forall m.
     MonadAff m
  => TestEnv m
  -> { account1 :: Address
     , provider :: Provider
     , trackMint :: SignalToken.TrackedToken
     , signalMarket :: Address
     , signalToken :: Address
     }
  -> m { trackMint :: SignalToken.TrackedToken
       , signalForSale :: SignalMarket.SignalForSale
       }
markSignalForSale { logger } { signalToken, signalMarket, trackMint, provider, account1 } = do
  -- marking for sale
  let txOpts = defaultTransactionOptions # _from ?~ account1
                                         # _gas ?~ embed 8000000
      s = unwrap trackMint
      _tokenId = s.tokenID
      _price = mkUIntN s256 1 -- this is ETH price
      signalApproveAction =
        SignalToken.approve (txOpts # _to ?~ signalToken) { to: signalMarket
                                                          , tokenId: _tokenId
                                                          }
      signalApproveFilter = eventFilter (Proxy :: Proxy SignalToken.Approval) signalToken
      forSaleAction =
        SignalMarket.forSale (txOpts # _to ?~ signalMarket) { _tokenId
                                                            , _price
                                                            }
      forSaleFilter = eventFilter (Proxy :: Proxy SignalMarket.SignalForSale) signalMarket
  -- approve minted signal
  signalApproval <- monitorUntil provider signalApproveAction signalApproveFilter
  logger $ "AUX: Signal approved for SignalMarket " <> show signalApproval
  -- mark signal as for sale
  signalForSale <- monitorUntil provider forSaleAction forSaleFilter
  logger $ "AUX: Signal for sale " <> show signalForSale
  pure { trackMint, signalForSale }
