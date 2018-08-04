{-# LANGUAGE OverloadedStrings #-}
module Melange.Service
  (
    runServer
  ) where

import           Data.Proxy
import qualified Melange.API.API             as API (API, MelangePool, server)
import           Melange.DB.Initialize       (computeConnectionString)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleCorsResourcePolicy)
import           Servant
import           Squeal.PostgreSQL.Pool

melangeAPI :: Proxy API.API
melangeAPI = Proxy

melange :: API.MelangePool -> Application
melange pool = serve melangeAPI (API.server pool)

localhostCors :: CorsResourcePolicy
localhostCors = simpleCorsResourcePolicy { corsOrigins = Just (["http://localhost:8080"], True)
                                         , corsRequestHeaders = ["Content-Type"] }

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just localhostCors)

runServer :: IO ()
runServer =
  getPool >>= run 8492 . corsMiddleware . logStdoutDev . melange

getPool :: IO API.MelangePool
getPool = do
  cs <- computeConnectionString
  createConnectionPool cs 1 10 5
