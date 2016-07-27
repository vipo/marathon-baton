{-# LANGUAGE OverloadedStrings #-}
module Baton.Controller(
  routes
)where

import           Baton.DockerRegistry
import           Baton.Runner
import           Baton.Types
import           Baton.Marathon
import           Baton.Versioning

import qualified Baton.Pages.Apps as PA
import qualified Baton.Pages.Deploy as PD
import qualified Baton.Pages.Report as PR

import           Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Ord as O
import qualified Data.Text.Lazy as LT

import           Text.Blaze.Html hiding (text)
import           Text.Blaze.Html.Renderer.Text (renderHtml)

import           Web.Scotty.Trans

import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

routes :: ScottyT LT.Text (ReaderT Configuration IO) ()
routes = do
  middleware logStdout
  middleware $ staticPolicy (noDots >-> addBase ".")
  get "/" $
    redirect "/apps"
  get "/apps" $ do
    conf <- lift ask
    apps <- liftIO $ dockerApps conf
    blaze $ PA.page (marathonUrl conf) $ localApps conf apps
  get "/deploy" $ do
    conf <- lift ask
    app <- readDockerApp
    let d = docker app
    tags <- liftIO $ listTags conf (registry d) (name d)
    blaze $ PD.page app $ orderVersions tags
  post "/run" $ do
    conf <- lift ask
    app <- readDockerApp
    uuid <- liftIO $ run conf app
    redirect $ LT.pack $ "/run/" ++ uuid
  get "/run/:uuid" $ do
    uuid <- param "uuid"
    blaze $ PR.page uuid

readDockerApp :: ActionT LT.Text (ReaderT Configuration IO) DockerApp
readDockerApp = do
  app <- param "app"
  im <- DockerImage <$> param "image" <*> param "registry" <*> param "version"
  return $ DockerApp app im

localApps :: Configuration -> [MarathonApp] -> [DockerApp]
localApps conf apps = do
  a <- L.sortBy (O.comparing appName) apps
  v <- M.maybeToList (versionDockerImage (dockerRegistries conf) (dockerImage a))
  return $ DockerApp (appName a) v

blaze = html . renderHtml
