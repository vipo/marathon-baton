{-# LANGUAGE OverloadedStrings #-}

module Baton.Pages.Apps(
  page
) where

import           Baton.Types
import           Baton.Pages.Commons

import           Control.Monad (forM_)

import qualified Data.Maybe as M
import qualified Data.List as L

import           Network.HTTP.Base (urlEncodeVars, urlEncode)

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
        H.th "Docker image"
        H.th "Image tag"
    H.tbody $
      forM_ apps appTableRow
  where
    appSiblings r i = map (\a -> ("app", appId a)) $
        filter (\a -> i == name (docker a) && r == registry (docker a)) apps
    appTableRow :: DockerApp -> H.Html
    appTableRow (DockerApp n (DockerImage i r t)) =
      H.tr $ do
        H.td $
          H.a ! A.href (toValue ( "/deploy?" ++ urlEncodeVars [
            ("app", n), ("image", i), ("version", t), ("registry", r)])
            ) $ toHtml n
        H.td $
          H.a ! A.href (toValue ( "/deploy?" ++
              -- Compose url manually, otherwise we get comma separated app query params
              L.intercalate "&" (map (\(k,v) -> concat [k, "=", urlEncode v]) (
              appSiblings r i ++ [("image", i), ("version", t), ("registry", r)] )))
            ) $ toHtml $ concat [r, "/", i]
        H.td $ toHtml t
