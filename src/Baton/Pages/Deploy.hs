{-# LANGUAGE OverloadedStrings #-}

module Baton.Pages.Deploy(
page
) where

import           Baton.Types
import           Baton.Pages.Header

import           Control.Monad (forM_)

import qualified Data.Maybe as M

import           Network.HTTP.Base (urlEncode)

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), toHtml, toValue)
import qualified Text.Blaze.Html5.Attributes as A

page :: DockerApp -> [String] -> H.Html
page (DockerApp n (DockerImage i r t)) versions =
  header "Deploy" $ do
    H.h1 "Marathon Application Deployment"
    H.form ! A.class_"pure-form pure-form-aligned" ! A.method "post" ! A.action "/run" $
      H.fieldset $ do
        H.div ! A.class_ "pure-control-group" $ do
          H.label ! A.for "app" $ "Application name"
          H.input ! A.readonly "readonly" ! A.name "app" ! A.id "app" ! A.value (toValue n)
        H.div ! A.class_ "pure-control-group" $ do
          H.label ! A.for "registry" $ "Docker registry"
          H.input ! A.readonly "readonly" ! A.name "registry" ! A.id "registry" ! A.value (toValue r)
        H.div ! A.class_ "pure-control-group" $ do
          H.label ! A.for "image" $ "Docker image"
          H.input ! A.readonly "readonly" ! A.name "image" ! A.id "image" ! A.value (toValue i)
        H.div ! A.class_ "pure-control-group" $ do
          H.label ! A.for "version" $ "Image tag"
          H.select ! A.name "version" ! A.id "version" $ do
            forM_ versions (versionOption t)
        H.div ! A.class_ "pure-controls" $
          H.button ! A.type_ "submit" ! A.class_ "pure-button pure-button-primary" $ "Deploy"

versionOption :: String -> String -> H.Html
versionOption curr ver =
  if curr == ver then
    H.option ! A.selected "" $ toHtml ver
  else
    H.option $ toHtml ver
