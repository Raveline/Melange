module Melange.Service
  (
    runServer
  ) where

import           Data.Proxy
import qualified Melange.API.API            as API (API, server, MelangePool)
import           Melange.DB.Initialize      (computeConnectionString)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Squeal.PostgreSQL.Pool

melangeAPI :: Proxy API.API
melangeAPI = Proxy

melange :: API.MelangePool -> Application
melange pool = serve melangeAPI (API.server pool)

runServer :: IO ()
runServer =
  getPool >>= run 8492 . melange

getPool :: IO API.MelangePool
getPool = do
  cs <- computeConnectionString
  createConnectionPool cs 1 10 5
