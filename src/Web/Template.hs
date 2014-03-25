{-# LANGUAGE OverloadedStrings #-}
module Web.Template where

import           Text.Blaze.Html
import           Data.Monoid
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


headWithTitle :: H.Markup -> Html
headWithTitle t = H.head $ do
    meta ! A.charset "utf-8"
    meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    meta ! A.name "description" ! A.content ""
    meta ! A.name "author" ! A.content "Schell Scivally"
    H.title t
    --  Bootstrap core CSS
    link ! A.href "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" ! A.rel "stylesheet"
    link ! A.href "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css" ! A.rel "stylesheet"
    --  Add custom CSS here
    H.style "body {margin-top: 60px;}"

wrapper :: H.Markup -> H.Markup -> Html
wrapper t content =
    docTypeHtml ! A.lang "en" $ do
        headWithTitle t
        body $ do
            nav ! A.class_ "navbar navbar-inverse navbar-fixed-top" $
                H.div ! A.class_ "container" $ do
                    H.div ! A.class_ "navbar-header" $ do
                        button ! A.type_ "button" ! A.class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-ex1-collapse" $ do
                            H.span ! A.class_ "sr-only" $ "Toggle navigation"
                            H.span ! A.class_ "icon-bar" $ mempty
                            H.span ! A.class_ "icon-bar" $ mempty
                            H.span ! A.class_ "icon-bar" $ mempty
                        a ! A.class_ "navbar-brand" ! A.href "/" $ "Orion ***"
                    --  Collect the nav links, forms, and other content for toggling
                    H.div ! A.class_ "collapse navbar-collapse navbar-ex1-collapse" $ ul ! A.class_ "nav navbar-nav" $ do
                        li $ a ! A.href "/login" $ "Login"
                    --  /.navbar-collapse
                --  /.container
            H.div ! A.class_ "container" $ H.div ! A.class_ "row" $ H.div ! A.class_ "col-lg-12" $ content
            --  /.container
            --  JavaScript
            script ! A.src "http://code.jquery.com/jquery-1.10.2.min.js" $ mempty
            script ! A.src "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js" $ mempty

loginOptions :: Html
loginOptions = H.div $ do
    p $ "Sign in using one of these services:"
    ul $ do
        li $ a ! A.href "/login/github" $ "Github"
        li $ a ! A.href "/login/github" $ "Github"
        li $ a ! A.href "/login/github" $ "Github"
