module App.Websocket where

import Prelude

import Control.Coroutine (Consumer, Producer, consumer, producer, pullFrom, runProcess, transform, ($~))
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Ord (abs)
import Data.String (replace, Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, killFiber, launchAff_)
import Effect.Aff.AVar.RW (AVarRW, AVarW)
import Effect.Aff.AVar.RW as AVar
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throw)
import Effect.Random (randomInt)
import Effect.Var (($=))
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode, decodeJSON, encodeJSON)
import Foreign.Object as FO
import Network.Ethereum.Web3 as Web3
import Network.Ethereum.Web3.Solidity (class DecodeEvent, decodeEvent)
import Partial.Unsafe (unsafePartial)
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
  { send :: AVarW UpdateSubscriptionMsg
  , receive :: Bus.BusR WebSocketMsg
  }

createWebSocket :: String -> Aff WebSocket
createWebSocket url' = do
    Tuple incomingBusR incomingBusW <- Bus.split <$> Bus.make
    outgoingAVarRW <- AVar.empty
    let outgoingAVarW = snd $ AVar.split outgoingAVarRW
    let url = (replace (Pattern "http") (Replacement "ws") url') <> "ws"
    log ("Connecting to websocket on  " <> url)
    liftEffect $ createSocket url incomingBusW outgoingAVarRW $ Milliseconds 1000.0
    pure { send: outgoingAVarW
         , receive: incomingBusR
         }
  where
    parseIncoming :: String -> Effect WebSocketMsg
    parseIncoming str = do
      case runExcept $ decodeJSON str of
        Left err -> throw $ show err
        Right n -> pure n

    createSocket :: String -> Bus.BusW WebSocketMsg -> AVarRW UpdateSubscriptionMsg -> Milliseconds -> Effect Unit
    createSocket url incomingBus outgoingAVar delayMS = do
      WS.Connection socket <- WS.newWebSocket (WS.URL url) []
      socket.onmessage $= \r -> do
        incoming <- parseIncoming <<< WS.runMessage <<< WS.runMessageEvent $ r
        launchAff_ $ Bus.write incoming incomingBus
      socket.onclose $= \_ -> do
        launchAff_ do
          delay delayMS
          liftEffect
            $ createSocket url incomingBus outgoingAVar
            $ over Milliseconds (_ + 500.0) delayMS
      socket.onopen $= \_-> do
        let sendPayload = encodeJSON >>> WS.Message >>> socket.send
        launchAff_ $ forever do
          msg <- AVar.take outgoingAVar
          liftEffect $ sendPayload msg

-------------------------

changeWSProducer
  :: SubscriptionID
  -> Bus.BusR WebSocketMsg
  -> Aff { changeProducer :: Producer Web3.Change Aff Unit
         , writeLoopCanceler :: Aff Unit
         }
changeWSProducer sId busR = do
  changeVar <- AVar.empty
  let writeLoop = forever do
        WebSocketMsg msg <- Bus.read busR
        void $ forkAff
          if msg.subscriptionID /= sId
            then pure unit
            else AVar.put msg.contents changeVar
      changeProducer = producer $ Left <$> AVar.take changeVar
  f <- forkAff writeLoop
  let writeLoopCanceler = do
        killFiber (error "producer ended") f
        AVar.kill (error "producer ended") changeVar
  pure {changeProducer, writeLoopCanceler}

changeConsumer
  :: forall e
   . Consumer {change :: Web3.Change, event :: e} Aff Unit
changeConsumer = consumer \i -> do
  log $ unsafeCoerce i
  pure Nothing

mkMonitor
  :: forall i ni e
   . DecodeEvent i ni e
  => WebSocket
  -> Web3.Filter e
  -> Consumer {change :: Web3.Change, event :: e} Aff Unit
  -> Aff (Aff Unit)
mkMonitor { send, receive } fltr consumer = do
  n <- liftEffect $ randomInt 1 2147483647
  let subscriptionID = "subs-" <> show (abs n)
      initSubscriptionMsg =
        Update $ Subscription
          { subscriptionID
          , filter: FilterE $ mkExists fltr
          }
      eventMapper = transform \change ->
        unsafePartial $ case decodeEvent change of
          Just (event :: e) -> {event, change}
  {changeProducer, writeLoopCanceler} <- changeWSProducer subscriptionID receive
  let process = runProcess $
          (consumer `pullFrom` void (changeProducer $~ eventMapper))
      cancelWithServer = AVar.put (Cancel subscriptionID) send
  AVar.put initSubscriptionMsg send
  f <- forkAff process
  pure do
    cancelWithServer
    killFiber (error $ "cleanup subscription " <> subscriptionID) f
    writeLoopCanceler
