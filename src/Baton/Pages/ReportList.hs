{-# LANGUAGE OverloadedStrings #-}
module Baton.Pages.ReportList (
  page
) where

import           Baton.Pages.Commons

import           Control.Monad (forM_)

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), toHtml, toValue)
import qualified Text.Blaze.Html5.Attributes as A

page :: [(String,String)] -- ^ (apps + uuids) of run
     -> H.Html
page uuids = header "Logs" $ do
  H.h1 "Logs"
  H.ul $ forM_ uuids (\(app, uid) -> H.li (H.a ! A.href (toValue ("/run/"++uid)) $ toHtml app))
