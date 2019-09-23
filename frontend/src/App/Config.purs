module App.Config where

import Prelude

import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Unsafe (unsafeFromJust)

-- TODO read from evn var instead or from server
config ::
  { foamTokenAddress :: Address
  }
config =
  { foamTokenAddress:unsafeFromJust "foamTokenAddress is valid address"
        $ (mkHexString "0x0123456789012345678901234567890123456789">>= mkAddress)
  }