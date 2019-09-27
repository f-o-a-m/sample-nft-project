module Test.End2EndSpec where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (ask)
import Effect.AVar as EAVar
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Network.Ethereum.Web3 (BytesN, UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, S32)
import Test.End2EndConfig (End2EndConfig)
import Test.Spec (SpecT)
import Test.Utils (go, mkSignalAttrGen)

type End2EndEnv m =
  { logger :: String -> m Unit -- @NOTE: use this for proper parallel logging
  , signalAttrGen :: m { geohash :: BytesN S32
                       , radius :: UIntN S256
                       }
  }

spec :: End2EndConfig -> SpecT Aff Unit Aff Unit
spec cfg = do
  uIntV <- liftEffect $ EAVar.new 10
  let signalAttrGen = mkSignalAttrGen uIntV
      env = { logger: \s -> ask >>= \logger -> liftAff $ logger s
            , signalAttrGen
            }
  go $ spec' cfg env

spec'
  :: forall m.
     MonadAff m
  => MonadError Error m
  => End2EndConfig
  -> End2EndEnv m
  -> SpecT m Unit Aff Unit
spec' cfg env@{ logger, signalAttrGen } = do
  pure unit
