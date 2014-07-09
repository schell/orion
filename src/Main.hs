{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           System.Remote.Monitoring
import           Web.Auth.OAuth2
import           Web.Auth.Service
import           Web.Orion
import           Web.Scotty.Trans
import           Web.Template
import           Web.Template.Renderer
import           Web.Session
import           Database
import           Text.Blaze.Html
import           Text.Blaze.Html5 hiding (param, object, map, b)
import qualified Data.Text.Lazy                  as LT
import qualified Data.ByteString.Lazy.Char8      as LB
import qualified Data.Map                        as Map


main :: IO ()
main = do
    _ <- forkServer "localhost" 9989

    orion $ do
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
            let Just key = oauth2serviceKey service
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

        --get "/link/:service" $ withAuthUser $ \u -> do
        --    service <- param "service"
        --    if service `elem` linkedServices u
        --    then blaze $ wrapper "Link Service" $ do
        --        h3 "Blarg"
        --        p "You've already linked that service."
        --    else do let key = serviceKey service
        --            baseUrl <- readCfg getCfgBaseUrl
        --            let call = concat [ baseUrl
        --                              , "/link/"
        --                              , serviceToString service
        --                              , "/complete"
        --                              ]
        --                call' = B.pack call
        --                key' = key{oauthCallback= Just call'}
        --            url <- prepareServiceLogin service key'
        --            redirect url

        --get "/link/:service/complete" $ withAuthUser $ \OrionUser{..} -> do
        --    dest    <- popRedirectDestination
        --    service <- param "service"
        --    eUdat   <- fetchRemoteUserData
        --    case eUdat of
        --        Left err   -> blaze $ authdWrapper "Link Error" $ do
        --                        h3 $ toHtml $ "Error linking " ++ serviceToString service
        --                        p $ toHtml $ LB.unpack err
        --        Right udat -> do mUser <- addAccountToUser service _ouId udat
        --                         case mUser of
        --                             Nothing -> blaze $ authdWrapper "Link Error" $ do
        --                                            h3 $ toHtml $ "Error linking " ++ serviceToString service
        --                                            p $ "Failed to add account."
        --                             Just _  -> redirect dest



popRedirectDestination :: ActionOM LT.Text
popRedirectDestination = do
    state  <- param "state"
    states <- readAuthStates
    modifyAuthStates $ Map.delete state
    case Map.lookup state states of
        Nothing   -> redirect "error/stateMismatch"
        Just dest -> return dest

