{-# LANGUAGE OverloadedStrings #-}
module Baton.Pages.Report(
  page
) where

import           Baton.Pages.Commons

import           System.Exit (ExitCode(..))

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), toHtml, toValue)
import qualified Text.Blaze.Html5.Attributes as A

page :: String -- ^ uuid of run
     -> H.Html
page uuid = header "Logs" $ do
  H.h1 "Logs"
  H.h2 "Status"
  H.h3 ! A.id "status" $ "Unknown"
  H.h2 "Stdout"
  H.textarea !
    A.style (toValue (style ++ "background-color: limegreen;")) !
    A.id "stdout" !
    A.readonly "readonly" $ ""
  H.h2 "Stderr"
  H.textarea !
    A.style (toValue (style ++ "background-color: indianred;")) !
    A.id "stderr" !
    A.readonly "readonly" $ ""
  H.script $ toHtml $ script (src "status") (src "stdout") (src "stderr")
  where
    src s = concat ["/run/", uuid, "/", s, ".txt"]

style :: String
style = "width: 100%; height: 30vh; font-family: monospace; "

script s o e = unlines [
    "mb_timer = window.setInterval('reloadIFrames();', 1000);"
  , "function scrollToBottom(id){$(id).scrollTop($(id)[0].scrollHeight);}"
  , "function reloadIFrames() {"
  , "$.get('" ++ s ++ "', function(d){$('#status').html(d); mb_status = d});"
  , "$.get('" ++ o ++ "', function(d){$('#stdout').html(d); scrollToBottom('#stdout')});"
  , "$.get('" ++ e ++ "', function(d){$('#stderr').html(d); scrollToBottom('#stderr')});"
  , "if (typeof mb_status == 'string' && mb_status.startsWith('Exit')) window.clearInterval(mb_timer)"
  , "}"
  ]
