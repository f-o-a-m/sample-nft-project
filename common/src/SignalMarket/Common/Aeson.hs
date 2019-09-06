
module SignalMarket.Common.Aeson where

import           Data.Aeson.Casing                (aesonDrop, camelCase)
import qualified Data.Aeson as AE

defaultAesonOptions :: String -> AE.Options
defaultAesonOptions prefix = aesonDrop (length prefix) camelCase