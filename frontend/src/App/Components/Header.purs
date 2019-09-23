module App.Components.Header where

import Prelude

import App.Config (config)
import App.Data.Token (Token(..))
import App.Error (printCallError, printWeb3Error)
import App.Ethereum.Provider (ConnectedState, partialStateEq, viewConnectedState)
import App.Ethereum.Provider as Provider
import Contracts.FoamToken as FoamToken
import Control.Lazy (fix)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Fiber, error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Etherium.TxOpts (txTo)
import Network.Ethereum.Web3 (ChainCursor(..), EventAction(..), event, eventFilter, runWeb3)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import Type.Proxy (Proxy(..))

component :: React.Component Props
component = React.createComponent "Header"

type Props =
  { providerState :: Provider.State
  }

type State = 
  { balance :: Maybe Token
  , loadingFiber :: Maybe (Fiber Void)
  }
type Self = React.Self Props State

reloadUserBalance :: Self -> ConnectedState -> Aff Unit
reloadUserBalance self con = fix \loop -> do
  res <- runWeb3 con.provider $ FoamToken.balanceOf (txTo config.foamTokenAddress) Latest {_owner: con.userAddress}
  case res of
    Left web3Err -> log (printWeb3Error web3Err) *> loop
    Right (Left callError) -> log (printCallError callError) *> loop
    Right (Right balance) -> liftEffect $ self.setState _ {balance = Just $ Token balance}

pullUserBalance :: Self -> ConnectedState -> Aff Void
pullUserBalance self con = fix \loop -> do
  res <- runWeb3 con.provider
    $ event (eventFilter Proxy config.foamTokenAddress)
    $ \(FoamToken.Transfer {from, to}) -> do
      when (con.userAddress == from || con.userAddress == to) do
        liftAff $ reloadUserBalance self con
      pure ContinueEvent
  either (log <<< printWeb3Error) pure res
  loop

header :: Props -> JSX
header = React.make component
  { initialState: { balance: Nothing, loadingFiber: Nothing } :: State
  , render
  , willUnmount
  , didUpdate
  }
  where
    didUpdate self {prevProps} = do
      let providerStateWasChanged = not partialStateEq self.props.providerState prevProps.providerState
      when (providerStateWasChanged) $ do
        for_ self.state.loadingFiber $ killFiber (error "Provider state has changed") >>> launchAff_
        for_ (viewConnectedState self.props.providerState) \con -> do
          fib <- launchAff $ (reloadUserBalance *> pullUserBalance) self con
          self.setState _ {loadingFiber = Just fib}

    willUnmount self = do
      for_ self.state.loadingFiber $ killFiber (error "will unmount") >>> launchAff_
    
    -- TODO make it look nice
    -- TODO show avatar from address
    -- TODO show proper network name
    render self@{state, props} = R.div_
      [ R.text "header"
      , R.br {}
      , R.text case props.providerState of
          Provider.NotInjected -> "NotInjected"
          Provider.PendingApproval -> "PendingApproval"
          Provider.Rejected _ -> "Rejected"
          Provider.Loading -> "Loading"
          Provider.Connected {userAddress} -> "Connected " <> show {userAddress}
          Provider.NotConnected {userAddress, currentNetwork} -> "NotConnected " <> show {userAddress, currentNetwork}
      , R.br {}
      , R.text $ "balance:" <> show state.balance
      ]

