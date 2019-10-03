module App.Components.Header (header, Props) where

import Prelude

import App.Components.Avatar (avatar)
import App.Components.Common (renderToken)
import App.Data.Contracts (Contracts(..), networkName)
import App.Data.ProviderState as ProviderState
import App.Data.Token (Token(..))
import App.Error (printCallError, printWeb3Error)
import App.Ethereum.Provider as Provider
import App.HTML (classy, maybeHtml)
import App.HTML.Canceler (pushCanceler, runCancelers)
import Contracts.FoamToken as FoamToken
import Control.Lazy (fix)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (fiberCanceler, launchAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Etherium.TxOpts (txTo)
import Network.Ethereum.Web3 (ChainCursor(..), EventAction(..), event, eventFilter, runWeb3)
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
  { balance :: Maybe Token
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
          res <- runWeb3 con.provider $ FoamToken.balanceOf (txTo (un Contracts con.contracts).foamToken ) Latest {account: con.userAddress}
          case res of
            Left web3Err -> log (printWeb3Error web3Err) *> loop
            Right (Left callError) -> log (printCallError callError) *> loop
            Right (Right balance) -> liftEffect $ self.setState _ {balance = Just $ Token balance}

    render :: Self -> JSX
    render self@{state, props} = classy R.div "Header"
      [ R.div_ case props.providerState of
          ProviderState.Unknown ->
            []
          ProviderState.NotInjected ->
            [R.text "Web3 provider is not injected!"]
          ProviderState.Injected ->
            [R.text "Web3 Connect Request was send."]
          ProviderState.Rejected _ ->
            [R.text "Web3 Connect Request was declined"]
          ProviderState.Enabled Provider.Loading _ _ ->
            [R.text "Loading user info"]
          ProviderState.Enabled (Provider.Connected {userAddress}) _ _ ->
            [ R.text "Hi "
            , avatar userAddress
            , R.text " you are connected!"
            , R.br {}
            , case state.balance of
                Nothing -> R.text "Loading balance"
                Just b -> R.text "Your balance is " <> renderToken b
            ]
          ProviderState.Enabled (Provider.NotConnected {userAddress}) _ (Contracts {networkId}) ->
            [ maybeHtml userAddress \ua -> R.text "Hey " <> avatar ua <> R.text " "
            , R.text $ "please connect to " <> networkName networkId
            ]
      ]

