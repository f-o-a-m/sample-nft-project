module App.Components.Header (header, Props) where

import Prelude

import App.Components.Avatar (avatar)
import App.Components.Common (renderToken)
import App.Data.ProviderState as ProviderState
import App.Data.Token (FOAM, Token(..), tokenFromBigNumber)
import App.Error (printCallError, printWeb3Error)
import App.Ethereum.Provider as Provider
import App.HTML (classy, maybeHtml)
import App.HTML.Canceler (pushCanceler, runCancelers)
import App.MarketClient.Types (Contracts(..), networkName)
import Contracts.FoamToken as FoamToken
import Control.Lazy (fix)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Deploy.Utils (unsafeFromJust)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, fiberCanceler, launchAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Etherium.TxOpts (txTo)
import Network.Ethereum.Web3 (ChainCursor(..), EventAction(..), event, eventFilter, runWeb3)
import Network.Ethereum.Web3.Api as Web3
import Network.Ethereum.Web3.Types (ETHER)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import Type.Proxy (Proxy(..))

type Props =
  { providerState :: ProviderState.State
  }

component :: React.Component Props
component = React.createComponent "Header"

type State =
  { balance :: Maybe {foam :: Token FOAM, eth :: Token ETHER}
  }

type Self = React.Self Props State

header :: Props -> JSX
header = React.make component
  { initialState: { balance: Nothing }
  , render
  , willUnmount
  , didUpdate
  }
  where
    willUnmount :: Self -> Effect Unit
    willUnmount self = runCancelers self

    didUpdate :: Self -> { prevProps :: Props, prevState :: State } -> Effect Unit
    didUpdate self {prevProps} = do
      let providerStateWasChanged = not ProviderState.partialEq self.props.providerState prevProps.providerState
      when (providerStateWasChanged) $ do
        self.setState _ {balance = Nothing}
        for_ (ProviderState.viewConnectedState self.props.providerState) \con -> do
          runCancelers self
          fib <- launchAff $ pollUserBalance self con
          pushCanceler self $ fiberCanceler fib

    pollUserBalance self con = do
      reloadUserBalance
      fix \loop -> do
        res <- runWeb3 con.provider
          $ void
          $ event (eventFilter Proxy (un Contracts con.contracts).foamToken )
          $ \t@(FoamToken.Transfer {from, to}) -> do
            when (con.userAddress == from || con.userAddress == to) do
              liftAff $ reloadUserBalance
            pure ContinueEvent
        either (log <<< printWeb3Error) pure res
        loop
      where
        reloadUserBalance = fix \loop -> do
          res <- runWeb3 con.provider do
            foamBalance <- fix \retry -> FoamToken.balanceOf (txTo (un Contracts con.contracts).foamToken ) Latest {account: con.userAddress} >>= case _ of
              Left callError ->do
                log (printCallError callError)
                liftAff $ delay (Milliseconds 3000.0)
                retry
              Right res -> pure res
            ethBalance <- Web3.eth_getBalance con.userAddress Latest
            pure
              { foam: Token foamBalance
              , eth: unsafeFromJust "eth_getBalance returned invalid result" $ tokenFromBigNumber ethBalance
              }

          case res of
            Left web3Err -> log (printWeb3Error web3Err) *> delay (Milliseconds 100.0) *> loop
            Right balance -> liftEffect $ self.setState _ {balance = Just balance}

    render :: Self -> JSX
    render self@{state, props} = classy R.div "Header"
      [ R.div_ case props.providerState of
          ProviderState.Unknown ->
            []
          ProviderState.NotInjected ->
            [R.text "Web3 provider is not injected!"]
          ProviderState.Injected {loading: false}->
            [R.text "Web3 Connect Request was send."]
          ProviderState.Injected {loading: true}->
            [R.text "Loading network configuration"]
          ProviderState.Rejected _ ->
            [R.text "Web3 Connect Request was declined"]
          ProviderState.Enabled {connectivity: Provider.Connected {userAddress}} ->
            [ R.text "Hi "
            , avatar userAddress
            , R.text " you are connected!"
            , R.br {}
            , case state.balance of
                Nothing -> R.text "Loading balance"
                Just {foam, eth} -> R.text "Your balance is " <> renderToken foam <> R.text " and " <> renderToken eth
            ]
          ProviderState.Enabled {connectivity: Provider.NotConnected {userAddress}, contracts: Contracts {networkId}} ->
            [ maybeHtml userAddress \ua -> R.text "Hey " <> avatar ua <> R.text " "
            , R.text $ "please connect to " <> networkName networkId
            ]
      ]

