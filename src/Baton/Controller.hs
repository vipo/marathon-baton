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
  middleware $ staticPolicy (noDots >-> addBase "runs" >-> hasPrefix "run")
  get "/" $
    redirect "/apps"
  get "/apps" $ do
    conf <- lift ask
    let marUrl = marathonUrl conf
    apps <- liftIO $ dockerApps marUrl
    blaze $ PA.page marUrl $ localApps conf apps
  get "/deploy" $ do
    app <- readDockerApp
    let d = docker app
    tags <- liftIO $ listTags (registry d) (name d)
    blaze $ PD.page app $ orderVersions tags
  post "/run" $ do
    conf <- lift ask
    app <- readDockerApp
    uuid <- liftIO $ run conf app
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
