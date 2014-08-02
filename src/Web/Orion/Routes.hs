{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Orion.Routes where

import           Web.Orion
import           Web.Orion.OAuth
import           Web.Orion.OAuth.Services
import           Web.Orion.Template
import           Web.Orion.Template.Renderer
import           Web.Orion.Session
import           Web.Orion.Database
import           Web.Scotty.Trans
import           Control.Monad.Reader
import           Text.Blaze.Html
import           Text.Blaze.Html5 hiding (param, object, map, b)
import qualified Data.Text.Lazy                  as LT
import qualified Data.ByteString.Lazy.Char8      as LB
import qualified Data.Map                        as Map


defaultOrionRoutes :: OrionM ()
defaultOrionRoutes = do
        createUserDatabaseIfNeeded

        get "/" $ (blaze $ authdWrapper "Home" "Home")
                  `ifAuthorizedOr`
                  (blaze $ wrapper "Home" "Home")

        get "/authCheck" $ do
            mU <- readUserCookie
            blaze $ wrapper "Auth Check" $ do
                h3 "Auth Check"
                pre $ toHtml $ show mU

        get "/user" $ withAuthUser $ \u -> blaze $ authdWrapper "User" $ userTable u

        get "/error/:err" $ do
            err <- fmap errorMessage $ param "err"
            blaze $ wrapper "Error" err

        get "/login" $ do
            dest <- defaultParam "redirect" ""
            (blaze $ authdWrapper "Login" $ loginOptions $ Just dest)
              `ifAuthorizedOr`
              (blaze $ wrapper "Login" $ loginOptions $ Just dest)

        get "/logout" $ do
            expireUserCookie
            blaze $ wrapper "Logout" $ p "You have been logged out."

        get "/login/:service" $ do
            service <- param "service"
            keys    <- lift $ asks _oKeys
            let Just key = oauth2serviceKey keys service
            url <- prepareServiceLogin service key
            redirect url

        get "/login/:service/complete" $ do
            dest <- popRedirectDestination
            eUser <- oauthenticate
            case eUser of
                Left err -> blaze $ wrapper "OAuth Error" $ do
                    h3 "Authentication Error"
                    p $ toHtml $ LB.unpack err
                Right u  -> do c <- futureCookieForUser u
                               writeUserCookie c
                               redirect dest

popRedirectDestination :: ActionOM LT.Text
popRedirectDestination = do
    state  <- param "state"
    states <- readAuthStates
    modifyAuthStates $ Map.delete state
    case Map.lookup state states of
        Nothing   -> redirect "error/stateMismatch"
        Just dest -> return dest

