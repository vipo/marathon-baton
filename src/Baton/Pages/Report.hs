{-# LANGUAGE OverloadedStrings #-}
module Baton.Pages.Report(
  page
) where

import           Baton.Pages.Header

import           System.Exit (ExitCode(..))

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), toHtml, toValue)
import qualified Text.Blaze.Html5.Attributes as A

page :: String -- ^ uuid of run
     -> H.Html
page uuid = header "Logs" $ do
  H.h1 "Logs"
  H.h2 "Stdout"
  H.iframe ! A.style style ! A.id "stdout" $ ""
  H.h2 "Stderr"
  H.iframe ! A.style style ! A.id "stderr" $ ""
  H.script $ toHtml $ script stdoutSrc stderrSrc
  where
    stdoutSrc = concat ["/run/", uuid, "/stdout.txt"]
    stderrSrc = concat ["/run/", uuid, "/stderr.txt"]

style = "width: 100%; height: 30vh;"

script o e = unlines [
    "window.setInterval('reloadIFrames();', 1000);"
  , "function reloadIFrames() {"
  , "document.getElementById('stdout').src='" ++ o ++ "';"
  , "document.getElementById('stderr').src='" ++ e ++ "';"
  , "}"
  ]
