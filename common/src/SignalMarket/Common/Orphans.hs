{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SignalMarket.Common.Orphans () where

import           Control.Monad.Catch           (MonadCatch (..))
import           Network.Ethereum.Api.Provider (Web3 (..))

deriving instance MonadCatch Web3
