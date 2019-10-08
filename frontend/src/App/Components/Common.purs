module App.Components.Common where

import Prelude

import App.Components.Avatar (avatar)
import App.Data.Collections (Cursor, LinkedCollection)
import App.Data.ProviderState (ConnectedState)
import App.Data.Radius (Radius(..))
import App.Data.Signal (Signal(..))
import App.Data.Token (class TokenName, Token(..), tokenFromBigNumber, tokenName)
import App.Data.User (User(..))
import App.Error (printWeb3Error)
import App.HTML (classy, maybeHtml, whenHtml)
import App.Route as Route
import Control.MonadZero as MZ
import Data.Array (length)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DF
import Data.Geohash (Geohash, geohashToLngLat)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Etherium.Tx as Tx
import Network.Ethereum.Core.BigNumber (divide)
import Network.Ethereum.Web3 (embed) as BN
import Network.Ethereum.Web3 (unUIntN)
import Network.Ethereum.Web3.Types (ETHER)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnitE18, ProxyTU(..), divider)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_, targetValue)
import React.Basic.Events (handler)
import Type.Proxy (Proxy(..))

fullDateFormat :: DF.Formatter
fullDateFormat
  = DF.MonthFull
  : DF.Placeholder " "
  : DF.DayOfMonth
  : DF.Placeholder ", "
  : DF.YearFull
  : Nil

renderDateTime :: DateTime -> String
renderDateTime = DF.format fullDateFormat

renderToken :: forall t. TokenName (Token t) => (Token t) -> JSX
renderToken (Token t) =
  let num = show $ unUIntN t `divide` divider (ProxyTU :: ProxyTU (MinorUnitE18 t))
  in R.text $ num <> " " <> tokenName (Proxy:: Proxy (Token t))

renderRadius :: Radius -> JSX
renderRadius (Radius n) = R.text $ show n <> " km"

renderGeoHash :: Geohash -> String
renderGeoHash = geohashToLngLat >>> \{lat, lng} -> show lat <> ", " <> show lng

renderBaseSignal :: Boolean -> Signal -> JSX
renderBaseSignal addLink (Signal s) = classy R.div "SignalContent"
  [ classy R.div "SignalContent-Left"
      [ if addLink
          then R.a
            { href: Route.href $ Route.Signal s.id
            , children: [ R.text $ renderGeoHash s.geohash ]
            }
          else  R.text $ renderGeoHash s.geohash
      , R.br {}
      , renderToken s.stake
      , R.text ", "
      , renderRadius s.radius
      ]
  , classy R.div "SignalContent-Right"
      [ R.text " owner "
      , avatar s.owner
      ]
  ]

type SignalState r =
  { signal :: Signal
  , tx :: Maybe Tx.Progress
  , price :: Maybe String
  | r
  }

renderSignal :: forall r.
  { state :: SignalState r
  , txSend :: Tx.Tx -> ConnectedState -> (Tx.Progress -> Effect Unit) -> Effect Unit
  , updateState :: (SignalState r -> SignalState r) -> Effect Unit
  , user :: User
  , addLink :: Boolean
  , details :: JSX
  }
  -> JSX
renderSignal {txSend, addLink, updateState, state, user, details} =
  let (Signal s) = state.signal
  in classy R.div "Signal"
    [ renderBaseSignal addLink state.signal
    , case user of
        UserGuest -> maybeHtml s.sale \{price} ->
          R.div_ [R.text $ "ON SALE FOR ", renderToken price]
        UserConnected con@{userAddress}
          | userAddress == s.owner -> case s.sale of
              Just {id, price} -> txOrElse state.tx $ React.fragment
                [ R.span_ [R.text $ "ON SALE FOR ", renderToken price, R.text " "]
                , R.button
                  { onClick: capture_ $ txSend
                      (Tx.UnSell id)
                      con
                      (\newTxSt -> updateState _ {tx = Just newTxSt})
                  , children: [ R.text "UnList" ]
                  }
                ]
              Nothing -> txOrElse state.tx $ React.fragment
                [ R.input {value: fromMaybe "" state.price, onChange: handler targetValue \p -> updateState _ {price = p}}
                , case state.price >>= parseToken of
                    Nothing -> R.button
                      { disabled: true
                      , children: [ R.text "Sell" ]
                      }
                    Just priceNum -> R.button
                      { onClick: capture_ $ txSend
                          (Tx.Sell s.id priceNum)
                          con
                          (\newTxSt -> updateState _ {tx = Just newTxSt})
                      , children: [ R.text "Sell" ]
                      }
                ]
          | otherwise -> maybeHtml s.sale \{id, price} -> txOrElse state.tx $ R.button
              { onClick: capture_ $ txSend
                  (Tx.Buy id price)
                  con
                  (\newTxSt -> updateState _ {tx = Just newTxSt})
              , children: [ R.text "Buy just for ", renderToken price ]
              }
    , details
    ]

parseToken :: forall t. String -> Maybe (Token t)
parseToken = Int.fromString >=> \num ->
  tokenFromBigNumber $ (BN.embed num) * divider (ProxyTU :: ProxyTU (MinorUnitE18 ETHER))

txOrElse :: Maybe Tx.Progress -> JSX -> JSX
txOrElse tx btn = classy R.div "Transaction" case tx of
  Nothing -> [btn]
  Just { finished, total, current } ->
    let progress = (show (length finished + 1) <> "/" <> show total)
    in case current of
      Tx.Submitting ->
        [ classy R.div "Transaction-status"
            [R.text $ progress <> " Submitting ..."]
        ]
      Tx.SubmittingFailed web3Err ->
        [ btn
        , classy R.div "Transaction-status Transaction-status--error"
            [R.text $ progress <> " SubmittingFailed " <> printWeb3Error web3Err]
        ]
      Tx.MiningStart txHash ->
        [ classy R.div "Transaction-status"
            [R.text $ progress <> " MiningStart " <> show txHash]
        ]
      Tx.MiningFailed transactionReceipt ->
        [ btn
        , classy R.div "Transaction-status Transaction-status--error"
            [R.text $ progress <> " MiningFailed " <> show transactionReceipt]
        ]
      Tx.MiningFinished txHash ->
        [ classy R.div "Transaction-status"
            [R.text $ progress <> " MiningFinished " <> show txHash]
        ]


renderLinkedCollection
  :: forall a
   . LinkedCollection a
  -> (Cursor -> Effect Unit)
  -> (Array a -> JSX)
  -> JSX
renderLinkedCollection {items, next, loading} loadMore renderItems = React.fragment
  [ renderItems items
  , classy R.div "LoadingMore"
      [ maybeHtml (MZ.guard (not loading) *> next) \cursor -> R.button
          { onClick: capture_ $ loadMore cursor
          , children: [ R.text "Load more" ]
          }
      , whenHtml loading \_ -> R.text "Loading ..."
      ]
  ]
