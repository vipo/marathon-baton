{-# LANGUAGE OverloadedStrings #-}

module Baton.Pages.Header(
  header
) where

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5.Attributes as A

header :: String -- ^ Title
       -> H.Html -- ^ Body
       -> H.Html
header t b =
  H.html $ do
    H.head $ do
      H.meta !
        A.name "viewport" !
        A.content "width=device-width, initial-scale=1.0"
      H.title (toHtml ("Marathon Baton: " ++ t))
      H.link !
        A.rel "stylesheet" !
        A.href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
      H.style $ toHtml css
    H.body b

css = unlines [
    ".button-xsmall { font-size: 50%; }"
  ]
