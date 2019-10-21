module App.API.WS (Connection, open, event) where

import Prelude

import App.API.Internal (apiBaseURL)
import Control.Coroutine as C
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..))
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (Filter)
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import App.Websocket (WebSocket, createWebSocket, mkMonitor)

newtype Connection = Connection WebSocket

open :: Aff Connection
open = Connection <$> createWebSocket apiBaseURL

event :: forall i ni e
  . DecodeEvent i ni e
  => Connection
  -> Filter e
  -> (e -> Effect Unit)
  -> Aff Canceler
event (Connection ws) fltr f = do
  cleanUp <- mkMonitor ws fltr $ C.consumer \m -> do
    liftEffect $ f m.event
    pure Nothing
  pure $ Canceler \_ -> cleanUp
