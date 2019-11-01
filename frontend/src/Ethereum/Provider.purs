module App.Ethereum.Provider
  ( getLegacyProvider
  , getEthereumProvider
  , enable
  , Provider'
  , kind ApprovalState
  , toWeb3Provider
  , Enabled
  , Unknown
  , live
  , Connectivity(..)
  ) where



import Prelude

import App.Error (printWeb3Error)
import App.MarketClient.Types (NetworkId(..))
import Control.Apply (lift2)
import Control.Lazy (fix)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds, delay, launchAff, launchAff_, parallel, sequential, try)
import Effect.Aff.AVar.RW as AVar
import Effect.Aff.BRef (BRefR)
import Effect.Aff.BRef as BRef
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Effect.Exception as Exception
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn2, runEffectFn3)
import Network.Ethereum.Web3 (Address, Provider, metamaskProvider, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, net_version)


data Connectivity
  = Connected { userAddress :: Address }
  | NotConnected { currentNetwork :: NetworkId, userAddress :: Maybe Address }

derive instance genericConnectivity :: Generic Connectivity _
instance eqConnectivity :: Eq Connectivity where eq = genericEq
instance showConnectivity :: Show Connectivity where show = genericShow

foreign import kind ApprovalState
foreign import data Enabled :: ApprovalState
foreign import data Unknown :: ApprovalState
newtype Provider' (st :: ApprovalState) = Provider' Provider

toWeb3Provider :: forall x. Provider' x -> Provider
toWeb3Provider (Provider' p) = p


enable :: Provider' Unknown -> Aff (Maybe (Provider' Enabled))
enable provider = do
  Tuple accountVarR accountVarW <- AVar.split <$> AVar.empty
  liftEffect $ runEffectFn3 enable_
    provider
    (mkEffectFn1 $ launchAff_ <<< flip AVar.put accountVarW)
    (mkEffectFn1 $ launchAff_ <<< flip AVar.kill accountVarW)
  try (AVar.read accountVarR) >>= case _ of
    Left err ->
      pure Nothing
    Right _ -> do
      pure $ Just $ Provider' $ toWeb3Provider provider

getEthereumProvider :: Effect (Maybe (Provider' Unknown))
getEthereumProvider = do
  provider <- runEffectFn2 getEthereumProvider_ Just Nothing
  pure $ map Provider' provider

getLegacyProvider :: Effect (Maybe (Provider' Enabled))
getLegacyProvider = Exception.try metamaskProvider >>= case _ of
  Left err -> pure Nothing
  Right provider -> pure $ Just $ Provider' provider


live ::
  { expectedNetwork :: NetworkId
  , reconnectionDelay :: Milliseconds
  , provider :: Provider' Enabled
  }
  -> Effect (Tuple (BRefR (Maybe Connectivity)) (Fiber Void))
live opts= do
  stateBRef <- BRef.make Nothing
  fib <- launchAff $ fix \loop -> do
    currentState <- liftEffect $ BRef.read stateBRef
    res <- runWeb3 (toWeb3Provider opts.provider) $ sequential $ lift2 Tuple
      (parallel $ net_version <#> NetworkId)
      (parallel $ eth_getAccounts <#> (_ !! 0))
    case res of
      Left err ->
        -- NOTE, if something goes wrong we ignore it, as it would be handled in some other place.
        liftEffect $ log $ "error during loadNetworkAndUserAddress: " <> printWeb3Error err
      Right (Tuple currentNetwork mbUserAddress) ->
        let
          newSt = Just $ case mbUserAddress of
            Nothing -> NotConnected { currentNetwork, userAddress: Nothing }
            Just userAddress
              | opts.expectedNetwork == currentNetwork -> Connected { userAddress }
              | otherwise -> NotConnected { currentNetwork, userAddress: Just userAddress }
        in unless (currentState == newSt) $ newSt `BRef.write` stateBRef
    delay $ opts.reconnectionDelay
    loop
  pure $ Tuple (fst $ BRef.split $ stateBRef) fib

foreign import getEthereumProvider_ :: forall res. EffectFn2
  (Provider -> res)
  res
  res

foreign import enable_ :: EffectFn3
  (Provider' Unknown)
  (EffectFn1 (Array Address) Unit)
  (EffectFn1 Error Unit)
  Unit
