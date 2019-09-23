module App.Ethereum.Provider
  ( State(..)
  , partialStateEq
  , Network
  , NotConnectedState
  , ConnectedState
  , viewConnectedState
  , getEnabledEthereumProviderState
  ) where



import Prelude

import App.Error (printWeb3Error)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Milliseconds, delay, launchAff_, parallel, sequential, try)
import Effect.Aff.AVar.RW as AVar
import Effect.Aff.BRef (BRefR)
import Effect.Aff.BRef as BRef
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, catchException)
import Effect.Uncurried (EffectFn1, EffectFn4, mkEffectFn1, runEffectFn4)
import Network.Ethereum.Web3 (Address, Provider, metamaskProvider, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, net_version)


-- | Initially state can be `NotInjected`or `PendingApproval`, for legacy
-- | providers it would be `Loading`.
-- |
-- | All possible state transitions:
-- | * PendingApproval -> Rejected err || Loading
-- | * Loading -> Connected a || NotConnected b
-- | * NotConnected a -> NotConnected b || Connected c
-- | * Connected a -> Connected b || NotConnected c
-- |
-- | `NotInjected` and `Rejected err` are terminal states.
data State
  = NotInjected
  | PendingApproval
  | Rejected Error
  | Loading
  | Connected (ConnectedState)
  | NotConnected (NotConnectedState)
type Network = String
type ConnectedState = { userAddress :: Address, provider :: Provider }
type NotConnectedState = { currentNetwork :: Network, userAddress :: Maybe Address }


partialStateEq :: State -> State -> Boolean
partialStateEq a b = case a, b of
  NotInjected, NotInjected -> true
  NotInjected, _ -> false
  Rejected _, Rejected _ -> true
  Rejected _, _ -> false
  PendingApproval, PendingApproval -> true
  PendingApproval, _ -> false
  Loading, Loading -> true
  Loading, _ -> false
  Connected s, Connected s' -> s.userAddress == s'.userAddress
  Connected s, _ -> false
  NotConnected s, NotConnected s' -> s' == s
  NotConnected s, _ -> false

viewConnectedState :: State -> Maybe ConnectedState
viewConnectedState (Connected s) = Just s
viewConnectedState _ = Nothing

-- https://medium.com/metamask/https-medium-com-metamask-breaking-change-injecting-web3-7722797916a8
getEnabledEthereumProviderState
  ::
    { expectedNetwork :: String
    , reconnectionDelay :: Milliseconds
    }
  -> Effect (BRefR State)
getEnabledEthereumProviderState {expectedNetwork, reconnectionDelay} = do
  Tuple accountVarR accountVarW <- AVar.split <$> AVar.empty
  runEffectFn4 getEnabledEthereumProvider_
    Just
    Nothing
    (mkEffectFn1 $ launchAff_ <<< flip AVar.put accountVarW)
    (mkEffectFn1 $ launchAff_ <<< flip AVar.kill accountVarW)
    >>= case _ of
      Nothing -> catchException (const $ pure Nothing) (Just <$> metamaskProvider) >>= case _ of
        Just provider -> do
          stateBRef <- BRef.make Loading
          launchAff_ $ connectionLoop provider stateBRef
          pure $ fst $ BRef.split $ stateBRef
        Nothing -> do
          stateBRef <- BRef.make NotInjected
          pure $ fst $ BRef.split $ stateBRef
      Just provider -> do
        stateBRef <- BRef.make PendingApproval
        launchAff_ $ try (AVar.read accountVarR) >>= case _ of
          Left err ->
            Rejected err `BRef.write` stateBRef
          Right _ -> do
            Loading `BRef.write` stateBRef
            connectionLoop provider stateBRef
        pure $ fst $ BRef.split $ stateBRef
  where
    connectionLoop provider stateBRef = do
      currentState <- liftEffect $ BRef.read stateBRef
      runWeb3 provider loadNetworkAndUserAddress >>= case _ of
        Left err ->
          -- NOTE, if something goes wrong we ignore it, as it would be handled in some other place.
          liftEffect $ log $ "error during loadNetworkAndUserAddress: " <> printWeb3Error err
        Right (Tuple currentNetwork mbUserAddress) ->
          let
            newSt = case mbUserAddress of
              Nothing -> NotConnected { currentNetwork, userAddress: Nothing }
              Just userAddress
                | expectedNetwork == currentNetwork -> Connected { userAddress, provider }
                | otherwise -> NotConnected { currentNetwork, userAddress: Just userAddress }
          in unless (currentState `partialStateEq` newSt) $ newSt `BRef.write` stateBRef
      delay $ reconnectionDelay
      connectionLoop provider stateBRef

    loadNetworkAndUserAddress = sequential $ Tuple
      <$> parallel net_version
      <*> parallel (eth_getAccounts <#> (_ !! 0))

foreign import getEnabledEthereumProvider_ :: forall res. EffectFn4
  (Provider -> res)
  res
  (EffectFn1 (Array Address) Unit)
  (EffectFn1 Error Unit)
  res

