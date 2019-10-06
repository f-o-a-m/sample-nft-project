module Test.Spec.Websocket where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Newtype (over)
import Data.String (replace, Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Var (($=))
import WebSocket as WS

type WebSocket = { send :: Bus.BusW String
                 , receive :: Bus.BusR String
                 }

createWebSocket :: String -> Aff WebSocket
createWebSocket url = do
    Tuple incomingBusR incomingBusW <- Bus.split <$> Bus.make
    Tuple outgoingBusR outgoingBusW <- Bus.split <$> Bus.make
    let url = (replace (Pattern "http") (Replacement "ws") url) <> "ws"
    liftEffect $ createSocket url incomingBusW outgoingBusR $ Milliseconds 1000.0
    pure { send: outgoingBusW
         , receive: incomingBusR
         }
  where
    parseIncoming str = do
      case A.jsonParser str >>= A.decodeJson of
        Left err -> throw err
        Right n -> pure n

    createSocket :: String -> Bus.BusW String -> Bus.BusR String -> Milliseconds -> Effect Unit
    createSocket url incomingBus outgoingBus delayMS = do
      WS.Connection socket <- WS.newWebSocket (WS.URL url) []
      socket.onmessage $= \r -> do
        incoming <- parseIncoming <<< WS.runMessage <<< WS.runMessageEvent $ r
        launchAff_ $ Bus.write incoming incomingBus
      socket.onclose $= \_ -> do
        launchAff_ do
          delay delayMS
          liftEffect
            $ createSocket url incomingBus outgoingBus
            $ over Milliseconds (_ + 500.0) delayMS
      socket.onopen $= \_-> do
        let sendPayload = A.encodeJson >>> A.stringify >>> WS.Message >>> socket.send
        launchAff_ $ forever do
          msg <- Bus.read outgoingBus
          liftEffect $ sendPayload msg
