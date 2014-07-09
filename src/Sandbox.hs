{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (div)
import Network.OAuth.Http.Request
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Consumer
import Network.OAuth.Http.Response
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import Crypto.BCrypt
import Data.Maybe
import Keys
import Web.Scotty
import Web.Template.Renderer
import Text.Blaze.Html5 hiding (param)
import Data.Text.Lazy as LT
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    authvar <- newMVar M.empty
    scotty 9988 $ routes authvar

routes :: MVar (M.Map String Token) -> ScottyM ()
routes authvar = do
    get "/" $ do
        salt <- liftIO $ (fmap . fmap) B.unpack $ genSaltUsingPolicy fastBcryptHashingPolicy
        loginUrl <- runOAuthM (fromApplication $ appWithSalt salt) $ do
            signedReqUrl <- signRq2 PLAINTEXT Nothing (reqUrl{ method = POST })
            token <- oauthRequest CurlClient signedReqUrl
            liftIO $ modifyMVar_ authvar $ return . M.insert (fromJust salt) token
            return $ authUrl token
            --cliAskAuthorization authUrl
            --liftIO $ putStrLn "going to accUrl"
            --b <- signRq2 PLAINTEXT Nothing accUrl >>= oauthRequest CurlClient
            --liftIO $ putStrLn "getting user data"
            --signRq2 HMACSHA1 (Just $ Realm "realm") srvUrl >>= serviceRequest CurlClient

        redirect $ LT.pack loginUrl 

    get "/callback" $ do
        verifier <- param "oauth_verifier"
        salt     <- param "salt"
        mToken   <- liftIO $ fmap (M.lookup salt) $ readMVar authvar
        case mToken of
            Nothing -> blaze $ div "Shit ain't der."
            Just t  -> do rsp <- runOAuthM t $ do
                               token <- getToken
                               putToken (injectOAuthVerifier verifier token)
                               _ <- signRq2 PLAINTEXT Nothing accUrl >>= oauthRequest CurlClient
                               signRq2 HMACSHA1 (Just $ Realm "realm") srvUrl >>= serviceRequest CurlClient
                          blaze $ pre $ toHtml $ LB.unpack $ rspPayload rsp



reqUrl :: Request
reqUrl = fromJust . parseURL $ "https://bitbucket.org/api/1.0/oauth/request_token"

accUrl :: Request
accUrl = fromJust . parseURL $ "https://bitbucket.org/api/1.0/oauth/access_token"

srvUrl :: Request
srvUrl = fromJust . parseURL $ "https://bitbucket.org/api/1.0/user"


authUrl :: Token -> String
authUrl = ("https://bitbucket.org/api/1.0/oauth/authenticate?oauth_token=" ++) . findWithDefault ("oauth_token", "ERROR") . oauthParams

app :: Application
app = appWithSalt Nothing

appWithSalt :: Maybe String -> Application
appWithSalt = Application consumerKey consumerSecret . URL . ("http://localhost:9988/callback" ++) . salt
    where consumerKey    = oauthConsumerKey bitbucketKey
          consumerSecret = oauthConsumerSecret bitbucketKey
          salt Nothing   = ""
          salt (Just s)  = "?salt=" ++ s
