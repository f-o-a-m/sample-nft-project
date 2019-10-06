module Test.Spec.Websocket where

import Prelude

import Control.Coroutine (runProcess)
import Control.Coroutine.Transducer (Transducer, awaitForever, toProcess, transform, yieldT, (=>=))
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (over)
import Data.String (replace, Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Var (($=))
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode, decodeJSON, encodeJSON)
import Foreign.Object as FO
import Network.Ethereum.Web3 as Web3
import Network.Ethereum.Web3.Solidity (class DecodeEvent, decodeEvent)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import WebSocket as WS

type SubscriptionID = String

newtype FilterE = FilterE (Exists Web3.Filter)

instance encodeFilterE :: Encode FilterE where
  encode (FilterE fe) = runExists encode fe

newtype Subscription = Subscription
  { filter         :: FilterE
  , subscriptionID :: SubscriptionID
  }

derive instance genericSubscription :: Generic Subscription _
instance encodeSubscription :: Encode Subscription where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

data UpdateSubscriptionMsg =
    Update Subscription
  | Cancel SubscriptionID

instance encodeUpdateSubscriptionMsg :: Encode UpdateSubscriptionMsg where
  encode (Update s) = encode $ FO.fromFoldable
    [ Tuple "tag" $ encode "Update"
    , Tuple "contents" $ encode s
    ]
  encode (Cancel sId) = encode $ FO.fromFoldable
    [ Tuple "tag" $ encode "Cancel"
    , Tuple "contents" $ encode sId
    ]

newtype WebSocketMsg = WebSocketMsg
  { subscriptionID :: SubscriptionID
  , contents       :: Web3.Change
  }

derive instance genericWebSocketMsg :: Generic WebSocketMsg _
instance decodeWebSocketMsg :: Decode WebSocketMsg where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

type WebSocket =
  { send :: Bus.BusW UpdateSubscriptionMsg
  , receive :: Bus.BusR WebSocketMsg
  }

createWebSocket :: String -> Aff WebSocket
createWebSocket url' = do
    Tuple incomingBusR incomingBusW <- Bus.split <$> Bus.make
    Tuple outgoingBusR outgoingBusW <- Bus.split <$> Bus.make
    let url = (replace (Pattern "http") (Replacement "ws") url') <> "/ws"
    log ("Connecting to websocket on  " <> url)
    liftEffect $ createSocket url incomingBusW outgoingBusR $ Milliseconds 1000.0
    pure { send: outgoingBusW
         , receive: incomingBusR
         }
  where
    parseIncoming :: String -> Effect WebSocketMsg
    parseIncoming str = do
      case runExcept $ decodeJSON str of
        Left err -> throw $ show err
        Right n -> do
          log $ unsafeCoerce n
          pure n

    createSocket :: String -> Bus.BusW WebSocketMsg -> Bus.BusR UpdateSubscriptionMsg -> Milliseconds -> Effect Unit
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
        let sendPayload = encodeJSON >>> WS.Message >>> socket.send
        launchAff_ $ forever do
          log "Waiting for bus"
          msg <- Bus.read outgoingBus
          log $ unsafeCoerce msg
          liftEffect $ sendPayload msg

-------------------------

changeWSProducer
  :: SubscriptionID
  -> Bus.BusR WebSocketMsg
  -> Transducer Void Web3.Change Aff Unit
changeWSProducer sId busR = do
  log "Waiting to reveive changes"
  WebSocketMsg msg <- lift $ Bus.read busR
  log "GOT CHANGE!"
  if msg.subscriptionID /= sId
    then do
      log "WASN't Matching :/"
      changeWSProducer sId busR
    else do
      log "IT WAS A MATCH!"
      yieldT msg.contents
      changeWSProducer sId busR

changeConsumer
  :: forall e
   . Transducer e Void Aff Unit
changeConsumer = awaitForever \a -> do
  log "succesfully decoded change:"
  log $ unsafeCoerce a

mkMonitor
  :: forall i ni e
   . (DecodeEvent i ni e)
  => SubscriptionID
  -> Web3.Filter e
  -> WebSocket
  -> Aff { process :: Transducer e Void Aff Unit -> Aff Unit
         , canceler :: Aff Unit
         }
mkMonitor subscriptionID fltr { send, receive } = do
  let initSubscriptionMsg =
        Update $ Subscription
          { subscriptionID
          , filter: FilterE $ mkExists fltr
          }
      eventMapper = awaitForever \change ->
        case decodeEvent change of
          Nothing -> log "Couldn't decode change!"
          Just (a :: e) -> do
            log "Decoded CHange!"
            yieldT a
      changeProducer = changeWSProducer subscriptionID receive
      process consumer =
        void $ runProcess $ toProcess
          ( changeProducer =>= eventMapper =>= consumer )
      canceler = do
        log "doing canceller"
        Bus.write (Cancel subscriptionID) send
  log "writing InitSubscription"
  Bus.write initSubscriptionMsg send
  log "wrote InitSubscription"
  pure { process
       , canceler
       }
