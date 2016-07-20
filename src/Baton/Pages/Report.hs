{-# LANGUAGE OverloadedStrings #-}
module Baton.Pages.Report(
  page
) where

import           Baton.Pages.Header

import           System.Exit (ExitCode(..))

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), toHtml, toValue)
import qualified Text.Blaze.Html5.Attributes as A

page :: (ExitCode, String, String) -- ^ result, stdout, stderr
     -> H.Html
page (code, stdout, stderr) = header "Report" $ do
  case code of
    ExitSuccess -> H.h1 ! A.style "color:green;" $ "OK"
    ExitFailure v -> H.h1 ! A.style "color:red;" $ toHtml ("Exit code: " ++ show v)
  H.form ! A.class_ "pure-form pure-form-aligned" $
    H.fieldset $ do
      H.div ! A.class_ "pure-control-group" $ do
        H.label ! A.for "stdout" $ "Stdout:"
        H.textarea ! A.id "stdout" ! A.rows "12" ! A.cols "120" ! A.readonly "readonly" $ toHtml stdout
      H.div ! A.class_ "pure-control-group" $ do
        H.label ! A.for "stderr" $ "Stderr:"
        H.textarea ! A.id "stderr" ! A.rows "12" ! A.cols "120" ! A.readonly "readonly" $ toHtml stderr
