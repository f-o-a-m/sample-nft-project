
module SignalMarket.Common.Aeson where

import qualified Data.Aeson        as AE
import           Data.Aeson.Casing (aesonDrop, camelCase)

defaultAesonOptions :: String -> AE.Options
defaultAesonOptions prefix = aesonDrop (length prefix) camelCase
