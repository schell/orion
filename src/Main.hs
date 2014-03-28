{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Keys
import           Crypto.BCrypt
import           Web.Orion
import           Web.Scotty.Trans
import           Web.Template
import           Web.Template.Renderer
import           Web.Session
import           Web.User
import           Network.OAuth.OAuth2
import           Control.Monad.Reader
import           Text.Blaze.Html
import           Text.Blaze.Html5 hiding (param, object, map, b)
import           Data.Aeson
import           Data.List (transpose)
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text                       as T
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as LB
import qualified Data.Map                        as Map


optionalParam :: Parsable a => LT.Text -> ActionOM (Maybe a)
optionalParam x = (fmap Just $ param x) `rescue` (const $ return Nothing)


defaultParam :: Parsable a => LT.Text -> a -> ActionOM a
defaultParam x def = do
    mP <- optionalParam x
    return $ case mP of
        Nothing -> def
        Just pm -> pm


main :: IO ()
main = orion $ do
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
        dest <- fmap ("/" ++) $ defaultParam "redirect" ""
        Just salt <- liftIO $ genSaltUsingPolicy fastBcryptHashingPolicy
        modifyAuthStates $ Map.insert salt $ LT.pack dest
        let query   = [("state", salt)]
        -- | TODO: Add in more services.
        service <- param "service"
        let authUrl  = authorizationUrl (serviceKey service) `appendQueryParam` query
            authUrl' = authUrl `appendQueryParam` serviceQuery service
        redirect $ bsToLT authUrl'

    get "/login/:service/complete" $ do
        service <- param "service"
        signInWith service


bsToLT :: B.ByteString -> LT.Text
bsToLT = LT.pack . B.unpack


serviceKey :: AuthService -> OAuth2
serviceKey Github   = githubKey
serviceKey Facebook = facebookKey


serviceQuery :: AuthService -> QueryParams
serviceQuery Github = []
serviceQuery Facebook = [("scope", "user_about_me,email")]


signInWith :: AuthService -> ActionOM ()
signInWith service = do
    let key = serviceKey service
    code'  <- param "code"
    state  <- param "state"
    states <- readAuthStates
    modifyAuthStates $ Map.delete state
    case Map.lookup state states of
        Nothing   -> redirect "error/stateMismatch"
        Just dest -> do
            let (url, body') = accessTokenUrl key code'
                query        = [("state", state)]
            ebs <- liftIO $ doSimplePostRequest url $ body' ++ query
            case ebs of
                Right bs -> loginUser service bs dest
                Left _   -> redirect "/error/decodingToken"


loginUser :: AuthService -> LB.ByteString -> LT.Text -> ActionOM ()
loginUser service bs dest = do
    let jsn = if service == Facebook then parseQuery bs
              else case decode bs of
                       Just b  -> b
                       Nothing -> object []
    eUser <- case fromJSON jsn of
                 Error _ -> redirect "/error/decodingToken"
                 Success t -> liftIO $ getUserInfo service t
    case eUser of
        Right u  -> do writeUserCookie u
                       liftIO $ putStrLn $ "Redirecting to " ++ LT.unpack dest
                       redirect dest
        Left _   -> do redirect "/error/decodingUser"


parseQuery :: LB.ByteString -> Value
parseQuery bs = do
    let ps = transpose $ map (T.split (=='=')) $ T.split (=='&') $ T.pack $ LB.unpack bs
    case ps of
        names:values:_ -> object $ zip names $ map toJSON values
        _ -> object []
