{-# LANGUAGE OverloadedStrings #-}

module Baton.Pages.Apps(
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

page :: String       -- ^ Marathon url
     -> [DockerApp]  -- ^ App data
     -> H.Html
page marUrl apps = header "Applications" $ do
  H.h1 "Known Marathon Applications"
  H.table ! A.class_ "pure-table pure-table-horizontal" $ do
    H.thead $
      H.tr $ do
        H.th "Application name"
        H.th "Registry"
        H.th "Docker image"
        H.th "Image tag"
        H.th ! A.colspan "2" $"Actions"
    H.tbody $
      forM_ apps appTableRow
  where
    appTableRow :: DockerApp -> H.Html
    appTableRow (DockerApp n (DockerImage i r t)) =
      H.tr $ do
        H.td $ toHtml n
        H.td $ toHtml r
        H.td $ toHtml i
        H.td $ toHtml t
        H.td $
          H.form ! A.action "/deploy" ! A.method "get" $ do
            H.input ! A.type_ "hidden" ! A.name "app" ! A.value (toValue n)
            H.input ! A.type_ "hidden" ! A.name "image" ! A.value (toValue i)
            H.input ! A.type_ "hidden" ! A.name "version" ! A.value (toValue t)
            H.input ! A.type_ "hidden" ! A.name "registry" ! A.value (toValue r)
            H.button ! A.class_ "button-xsmall pure-button-primary pure-button" $ "New version"
        H.td $
          H.form ! A.action (toValue (marUrl ++ "/ui/#/apps/" ++ urlEncode n)) !
            A.method "get" $
            H.button !
              A.class_ "button-xsmall button-success pure-button" $ "Marathon"
