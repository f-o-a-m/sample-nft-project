module SignalMarket.Server.Server (server, mkApplication) where

import           Control.Monad.Reader                 (asks)
import           Data.Proxy
import qualified Katip                                as K
import           Servant.API                          ((:<|>) (..))
import           Servant.Server
import           SignalMarket.Common.Config.Utils     (makeConfig)
import           SignalMarket.Server.API
import           SignalMarket.Server.API.Types        ()
import           SignalMarket.Server.Application      (AppHandler,
                                                       runAppHandler)
import           SignalMarket.Server.Config           (AppConfig (..),
                                                       Contracts, mkAppConfig)
import           SignalMarket.Server.Server.Config    (configServer)
import           SignalMarket.Server.Server.FoamToken (foamTokenServer)

--------------------------------------------------------------------------------

server :: AppConfig -> Server API
server cfg = hoistServerWithContext api (Proxy :: Proxy '[]) (runAppHandler cfg) $
       configServer
  :<|> foamTokenServer


mkApplication :: AppConfig -> Application
mkApplication cfg = serveWithContext api EmptyContext $ server cfg
