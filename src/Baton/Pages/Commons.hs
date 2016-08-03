{-# LANGUAGE OverloadedStrings #-}

module Baton.Pages.Commons(
    header
  , marathonButton
) where

import           Network.HTTP.Base (urlEncode)

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), toHtml, toValue)
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
        A.href "///yui.yahooapis.com/pure/0.6.0/pure-min.css"
      H.script !
        A.src "///code.jquery.com/jquery-3.1.0.min.js" $ ""
      H.style $ toHtml css
    H.body $
      H.div ! A.class_ "pure-g" $ do
        H.div ! A.class_ "pure-u-2-24" $ ""
        H.div ! A.class_ "pure-u-20-24" $ b
        H.div ! A.class_ "pure-u-2-24" $ ""

css = unlines [
    ".button-xsmall { font-size: 70%; }"
  ]

marathonButton :: String  -- ^ Marathon Url
               -> String  -- ^ Name
               -> H.Html
marathonButton marUrl name =
  H.a !
    A.class_ "button-xsmall pure-button" !
    A.href (toValue (marUrl ++ "/ui/#/apps/" ++ urlEncode name)) $ "See in Marathon"