module App.Components.Common where

import Prelude

import App.Data.Collections (Cursor, LinkedCollection)
import App.Data.ProviderState (ConnectedState)
import App.Data.Signal (Signal(..))
import App.Data.Token (Token(..), tokenFromBigNumber, tokenToBigNumber)
import App.Data.User (User(..))
import App.Error (printWeb3Error)
import App.HTML (maybeHtml, whenHtml)
import App.Route as Route
import Control.MonadZero as MZ
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Etherium.Tx as Tx
import Network.Ethereum.Core.BigNumber (divide)
import Network.Ethereum.Web3 (Ether, fromMinorUnit, unUIntN)
import Network.Ethereum.Web3 (embed) as BN
import Network.Ethereum.Web3.Types (ETHER)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnitE18, ProxyTU(..), divider)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_, targetValue)
import React.Basic.Events (handler)


renderToken :: Token -> JSX
renderToken (Token t) =
  let num = show $ unUIntN t `divide` divider (ProxyTU :: ProxyTU Ether)
  in R.text $ num <> " ETH"

renderBaseSignal :: Signal -> JSX
renderBaseSignal (Signal s) = React.fragment
  [ R.div_ $ pure $ R.a
      { href: Route.href $ Route.Signal s.id
      , children: [ R.text $ show s.geohash ]
      }
  , R.div_ [R.text "stake: ", renderToken s.stake]
  , R.div_ [R.text $ "radius: " <> show s.radius]
  , R.div_ [R.text $ "owner: " <> show s.owner]
  ]

type SignalState r = {signal :: Signal, tx :: Maybe Tx.Status, price :: Maybe String | r }

renderSignal :: forall r.
  { state :: SignalState r
  , txSend :: Tx.Tx -> ConnectedState -> (Tx.Status -> Effect Unit) -> Effect Unit
  , updateState :: (SignalState r -> SignalState r) -> Effect Unit
  , user :: User
  }
  -> JSX
renderSignal {txSend, updateState, state, user} =
  let (Signal s) = state.signal
  in R.div_
    [ renderBaseSignal state.signal
    , case user of
        UserGuest -> maybeHtml s.sale \{price} ->
          R.div_ [R.text $ "ON SALE FOR ", renderToken price]
        UserConnected con@{userAddress}
          | userAddress == s.owner -> case s.sale of
              Just {id} -> txOrElse state.tx $ R.button
                { onClick: capture_ $ txSend
                    (Tx.UnSell id)
                    con
                    (\newTxSt -> updateState _ {tx = Just newTxSt})
                , children: [ R.text "UnList" ]
                }
              Nothing -> txOrElse state.tx $ React.fragment
                [ R.input {value: fromMaybe "" state.price, onChange: handler targetValue \p -> updateState _ {price = p}}
                , case state.price >>= parseToken of
                    Nothing -> R.button
                      { disabled: true
                      , children: [ R.text "Sell" ]
                      }
                    Just priceNum -> R.button
                      { onClick: capture_ $ txSend
                          (Tx.Sell s.id
                            priceNum
                          )
                          con
                          (\newTxSt -> updateState _ {tx = Just newTxSt})
                      , children: [ R.text "Sell" ]
                      }
                ]
          | otherwise -> maybeHtml s.sale \{id, price} -> txOrElse state.tx $ R.button
              { onClick: capture_ $ txSend
                  (Tx.Buy id $ fromMinorUnit $ tokenToBigNumber price)
                  con
                  (\newTxSt -> updateState _ {tx = Just newTxSt})
              , children: [ R.text "Buy" ]
              }
    ]

parseToken :: String -> Maybe Token
parseToken = Int.fromString >=> \num ->
  tokenFromBigNumber $ (BN.embed num) * divider (ProxyTU :: ProxyTU (MinorUnitE18 ETHER))

txOrElse :: Maybe Tx.Status -> JSX -> JSX
txOrElse tx btn = React.fragment case tx of
  Nothing -> [btn]
  Just tx' -> case tx' of
    Tx.Submitting ->
      [ R.text "Submitting"
      ]
    Tx.SubmittingFailed web3Err ->
      [ btn
      , R.text $ "SubmittingFailed " <> printWeb3Error web3Err
      ]
    Tx.MiningStart txHash ->
      [ R.text $ "MiningStart " <> show txHash
      ]
    Tx.MiningFailed transactionReceipt ->
      [ btn
      , R.text $ "MiningFailed " <> show transactionReceipt
      ]
    Tx.MiningFinished txHash ->
      [ btn
      , R.text $ "MiningFinished " <> show txHash
      ]


renderLinkedCollection
  :: forall a
   . LinkedCollection a
  -> (Cursor -> Effect Unit)
  -> (Array a -> JSX)
  -> JSX
renderLinkedCollection {items, next, loading} loadMore renderItems = React.fragment
  [ renderItems items
  , maybeHtml (MZ.guard (not loading) *> next) \cursor -> R.button
      { onClick: capture_ $ loadMore cursor
      , children: [ R.text "Load more" ]
      }
  , whenHtml loading \_ -> R.text "Loading ..."
  ]
