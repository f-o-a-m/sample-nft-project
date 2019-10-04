module App.Data.ProviderState where

import Prelude

import App.Data.Contracts (Contracts)
import App.Ethereum.Provider (Connectivity(..), Provider', Unknown)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (Address, Provider)


data State
  = Unknown
  | NotInjected
  | Injected { loading :: Boolean }
  | Rejected (Provider' Unknown)
  | Enabled Connectivity Provider Contracts


type ConnectedState = { userAddress :: Address, provider :: Provider, contracts :: Contracts }

viewConnectedState :: State -> Maybe ConnectedState
viewConnectedState (Enabled (Connected {userAddress}) provider contracts) = Just {userAddress, provider, contracts}
viewConnectedState _ = Nothing


partialEq :: State -> State -> Boolean
partialEq a b = case a, b of
  Unknown, Unknown -> true
  Unknown, _ -> false
  NotInjected, NotInjected -> true
  NotInjected, _ -> false
  Injected c, Injected c' -> c' == c
  Injected _, _ -> false
  Rejected _, Rejected _ -> true
  Rejected _, _ -> false
  Enabled c _ _, Enabled c' _ _ -> c' == c
  Enabled c _ _, _ -> false
