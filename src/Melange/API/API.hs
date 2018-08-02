{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Melange.API.API
  (
    API
  , server
  , MelangePool
  ) where

import           Control.Monad.Reader       (Reader, runReader)
import           Data.Proxy
import           Data.Time
import           Generics.SOP.BasicFunctors
import           Melange.DB.Schema
import           Melange.Model.External
import           Servant
import           Servant.Server
import           Squeal.PostgreSQL.Pool
import           Squeal.PostgreSQL.PQ

type MelangePool = Pool (K Connection Schema)
type MelangeHandler a = Reader MelangePool a

type API =
  "melange" :> ("boards" :> (NewBoard :<|> GetBoards))
             :<|> "board" :> Capture "date" Day :> (GetBoard :<|> PatchBoard :<|> DeleteBoard)

type NewBoard = ReqBody '[JSON] Board :> PostNoContent '[JSON] NoContent
type GetBoards = "all" :> Capture "page" Int :> Get '[JSON] [Board]
type GetBoard = Get '[JSON] Board
type PatchBoard = ReqBody '[JSON] Board :> PatchNoContent '[JSON] NoContent
type DeleteBoard = DeleteNoContent '[JSON] NoContent

melangeAPI :: Proxy API
melangeAPI = Proxy

server :: MelangePool -> Server API
server pool =
  let nat x = pure $ runReader x pool
      plural = (addBoard :<|> getBoards)
      singular day = (getBoard day :<|> patchBoard day :<|> deleteBoard day)
  in hoistServer melangeAPI nat (plural :<|> singular)

addBoard :: Board -> MelangeHandler NoContent
addBoard _ = pure NoContent

getBoards :: Int -> MelangeHandler [Board]
getBoards = undefined

getBoard :: Day -> MelangeHandler Board
getBoard = undefined

patchBoard :: Day -> Board -> MelangeHandler NoContent
patchBoard _ _ = pure NoContent

deleteBoard :: Day -> MelangeHandler NoContent
deleteBoard _ = pure NoContent
