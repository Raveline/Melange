{-# LANGUAGE OverloadedStrings #-}
module Melange.Service
  (
    runServer
  ) where

import           Data.Proxy
import qualified Melange.API.API                      as API (API, MelangePool,
                                                              server)
import           Melange.DB.Initialize                (computeConnectionString,
                                                       fetchEnv)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..),
                                                       cors,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Squeal.PostgreSQL.Pool

melangeAPI :: Proxy API.API
melangeAPI = Proxy

getStaticDirectory :: IO FilePath
getStaticDirectory = fetchEnv "MELANGE_STATIC_FILES"

melange :: FilePath -> API.MelangePool -> Application
melange staticDir = serve melangeAPI . API.server staticDir

localhostCors :: CorsResourcePolicy
localhostCors = simpleCorsResourcePolicy { corsOrigins = Just (["http://localhost:8080"], True)
                                         , corsMethods = ["GET", "HEAD", "POST", "DELETE", "PATCH"]
                                         , corsRequestHeaders = ["Content-Type"] }

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just localhostCors)

runServer :: IO ()
runServer = do
  pool <- getPool
  staticDir <- getStaticDirectory
  run 8492 . corsMiddleware . logStdoutDev $ melange staticDir pool

getPool :: IO API.MelangePool
getPool = do
  cs <- computeConnectionString
  createConnectionPool cs 1 10 5
