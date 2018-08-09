{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Melange.API.API
  (
    API
  , server
  , MelangePool
  ) where

import           Control.Exception.Safe        hiding (Handler)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Data.Proxy
import           Data.Time
import           Generics.SOP.BasicFunctors
import           Melange.DB.Insertions
import           Melange.DB.Schema
import           Melange.DB.Selections
import           Melange.DB.Deletions
import           Melange.DB.Types
import           Melange.Model
import           Melange.View.Index
import           Network.HTTP.Media            ((//), (/:))
import           Servant
import           Squeal.PostgreSQL.Pool
import           Squeal.PostgreSQL.PQ
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

type MelangePool = Pool (K Connection Schema)
type MelangeHandler = ReaderT MelangePool Handler

withPool :: PoolPQ Schema IO a -> MelangeHandler a
withPool q = do
  pool <- ask
  liftIO $ runPoolPQ q pool

withPoolE :: QueryException -> ServantErr -> PoolPQ Schema IO a -> MelangeHandler a
withPoolE qe onError query = do
  res <- try (withPool query)
  case res of
      Right x -> pure x
      Left e -> if e == qe then throwError onError else throwError err500

type API =
  "melange" :> (Plurals :<|> Singulars :<|> HomePage)

type Plurals = ("boards" :> (NewBoard :<|> GetBoards))
type Singulars = ("board" :> Capture "date" Day :> (GetBoard :<|> PatchBoard :<|> DeleteBoard))

type HomePage = Get '[HTML] IndexPage
type NewBoard = ReqBody '[JSON] BoardCreation :> PostNoContent '[JSON] NoContent
type GetBoards = "all" :> Capture "page" Int :> Get '[JSON] [Board]
type GetBoard = Get '[JSON] (Maybe Board)
type PatchBoard = ReqBody '[JSON] Board :> PatchNoContent '[JSON] NoContent
type DeleteBoard = DeleteNoContent '[JSON] NoContent

melangeAPI :: Proxy API
melangeAPI = Proxy

server :: MelangePool -> Server API
server pool =
  let nat x = runReaderT x pool
      plural = (addBoard :<|> getBoards)
      singular day = (getBoard day :<|> patchBoard day :<|> deleteBoard day)
  in hoistServer melangeAPI nat (plural :<|> singular :<|> renderHome)

renderHome :: MelangeHandler IndexPage
renderHome =
  IndexPage <$> withPool getLatestBoard

addBoard :: BoardCreation -> MelangeHandler NoContent
addBoard b =
  let onError = err422 { errBody = "There is a already a board with this date" }
  in NoContent <$ withPoolE AlreadyExists onError (newBoard b)

getBoards :: Int -> MelangeHandler [Board]
getBoards = undefined

getBoard :: Day -> MelangeHandler (Maybe Board)
getBoard =
  withPool . getBoardByDay

patchBoard :: Day -> Board -> MelangeHandler NoContent
patchBoard d b =
  let onError = err404 { errBody = "No board to update with this date" }
  in NoContent <$ withPoolE UpdateNonExistingEntity onError (updateBoard d b)

deleteBoard :: Day -> MelangeHandler NoContent
deleteBoard d =
  let onError = err404 { errBody = "No board to delete with this date" }
      removeProcess = removeBoardAtDay DeleteNonExistingEntity d
  in NoContent <$ withPoolE DeleteNonExistingEntity onError removeProcess

data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToMarkup a => MimeRender HTML a where
    mimeRender _ = renderHtml . Text.Blaze.Html.toHtml
instance MimeRender HTML Html where
    mimeRender _ = renderHtml
