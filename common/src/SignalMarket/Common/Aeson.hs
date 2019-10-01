
module SignalMarket.Common.Aeson where

import qualified Data.Aeson        as AE
import           Data.Aeson.Casing (aesonDrop, camelCase)

-- | Aeson options for generically deriving instances. The options
-- | allow you to remove field prefixes and camalCase.
-- e.g. in
--  data Foo = Foo
--    { fooName :: String
--    , fooData :: Int
--    }
--  using 'defaultAesonOptions "foo"' results in field names
--  "name" and "data".
defaultAesonOptions :: String -> AE.Options
defaultAesonOptions prefix = aesonDrop (length prefix) camelCase
