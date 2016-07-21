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
  H.form ! A.class_ "pure-form pure-form-aligned" $
    H.fieldset $ do
      H.div ! A.class_ "pure-control-group" $ do
        H.label ! A.for "stdout" $ "Stdout:"
        H.iframe ! A.src ( toValue (concat ["/run/", uuid, "/stdout.txt"])) $ ""
        --H.textarea ! A.id "stdout" ! A.rows "12" ! A.cols "120" ! A.readonly "readonly" $ toHtml stdout
      H.div ! A.class_ "pure-control-group" $ do
        H.label ! A.for "stderr" $ "Stderr:"
        H.iframe ! A.src ( toValue (concat ["/run/", uuid, "/stderr.txt"])) $ ""
        --H.textarea ! A.id "stderr" ! A.rows "12" ! A.cols "120" ! A.readonly "readonly" $ toHtml stderr
