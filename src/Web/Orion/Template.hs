{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Web.Orion.Template where

import           Network.HTTP.Types.Status
import           Data.Monoid
import           Data.List ((\\))
import           Text.Blaze.Html
import           Text.Blaze.Html5
import           Web.Orion.User
import           Web.Orion.Types
import           Web.Orion.OAuth.Services
import           Control.Monad
import           Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


headWithTitle :: Html -> Html
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

wrapper :: Html -> Html -> Html
wrapper = wrapperWithNav loggedOutNav

authdWrapper :: Html -> Html -> Html
authdWrapper = loggedIn

loggedIn :: Html -> Html -> Html
loggedIn = wrapperWithNav loggedInNav

wrapperWithNav :: Html -> Html -> Html -> Html
wrapperWithNav navbar t content =
    docTypeHtml ! A.lang "en" $ do
        headWithTitle t
        body $ do
            navbar
            H.div ! A.class_ "container" $ H.div ! A.class_ "row" $ H.div ! A.class_ "col-lg-12" $ content
            script ! A.src "http://code.jquery.com/jquery-1.10.2.min.js" $ mempty
            script ! A.src "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js" $ mempty

navFromPairs :: [(AttributeValue, Html)] -> [(AttributeValue, Html)] -> Html
navFromPairs leftLinks rightLinks =
    nav ! A.class_ "navbar navbar-inverse navbar-fixed-top" $
        H.div ! A.class_ "container" $ do
            H.div ! A.class_ "navbar-header" $ do
                button ! A.type_ "button" ! A.class_ "navbar-toggle"
                       ! dataAttribute "toggle" "collapse"
                       ! dataAttribute "target" ".navbar-ex1-collapse" $ do
                    H.span ! A.class_ "sr-only" $ "Toggle navigation"
                    forM_ leftLinks $ \(href', html') ->
                        H.span ! A.class_ "icon-bar" $ a ! A.href href' $ html'
                    forM_ rightLinks $ \(href', html') ->
                        H.span ! A.class_ "icon-bar" $ a ! A.href href' $ html'
                a ! A.class_ "navbar-brand" ! A.href "/" $ "Orion***"
            --  Collect the nav links, forms, and other content for toggling
            H.div ! A.class_ "collapse navbar-collapse navbar-ex1-collapse" $ do
                ul ! A.class_ "nav navbar-nav navbar-left" $
                    forM_ leftLinks $ \(href', html') -> li $ a ! A.href href' $ html'
                ul ! A.class_ "nav navbar-nav navbar-right" $
                    forM_ rightLinks $ \(href', html') -> li $ a ! A.href href' $ html'

loggedOutNav :: Html
loggedOutNav = navFromPairs [] [("/login", "Login")]

loggedInNav :: Html
loggedInNav = navFromPairs [("/user", "User")]  [("/logout", "Logout")]

loginOptions :: Maybe String -> Html
loginOptions mParam = flip (maybe "/") mParam $ \redir -> H.div $ do
    p $ "Sign in using one of these services:"
    ul $ do
        li $ a ! A.href (toValue $ "/login/github?redirect=" ++ redir) $ "Github"
        li $ a ! A.href (toValue $ "/login/facebook?redirect=" ++ redir) $ "Facebook"

errorMessage :: String -> Html
errorMessage "stateMismatch" = do
    h3 "State mismatch"
    p "It looks like there was an error communicating with the login service. \
       \You should make sure you are on a secure network as a man in the middle \
       \attack may be occurring."
errorMessage err = do
    h3 "Unknown error"
    p "An unknown error occured."
    pre $ toHtml err

errorStatus :: Status -> Html
errorStatus stat = do
    h3 $ toHtml $ show $ statusCode stat
    p $ toHtml $ B.unpack $ statusMessage stat

userTable :: OrionUser -> Html
userTable o@OrionUser{..} = do
    let services = allServices \\ linkedServices o
    h3 "User Data"
    dl $ do
        dt "User Id"
        dd $ toHtml $ show _ouId
        dt "User Level"
        dd $ toHtml $ show _ouAcl
    h4 "Linked Accounts"
    table ! A.class_ "table" $ do
        thead $ do
            th $ "Account Type"
            th $ "Account Id"
            th $ "Login"
            th $ "Name"
            th $ "Email"
            th $ "Has Token"
        tbody $ forM_ _ouAccounts $ \OrionAccount{..} -> tr $ do
            td $ toHtml $ show _accService
            td $ toHtml _accId
            td $ toHtml _accLogin
            td $ toHtml _accName
            td $ toHtml _accEmail
            td $ toHtml $ isJust _accToken
    h4 "Link Another Account"
    let serviceLink s = a ! A.href (toValue $ serviceHref s) $ toHtml $ show s
        serviceHref s = concat ["/login/", s2s s, "?redirect=user"]
        s2s = serviceToString
    ul $ forM_ services $ li . serviceLink
