{-# LANGUAGE OverloadedStrings #-}
module Web.Orion.OAuth where

import           Web.Orion.Types
import           Web.Orion
import           Web.Orion.Session
import           Web.Orion.Database
import           Web.Orion.OAuth.Services
import           Web.Parsers
import           Web.Scotty.Trans
import           Network.OAuth.OAuth2
import           Network.HTTP.Conduit
import           Crypto.BCrypt
import           Control.Monad.Reader
import           Data.Aeson
import           Data.List (transpose)
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text                       as T
import qualified Data.ByteString.Lazy.Char8      as LB
import qualified Data.ByteString.Char8           as B
import qualified Data.Map                        as Map


prepareServiceLogin :: AuthService -> OAuth2 -> ActionOM LT.Text
prepareServiceLogin service key = do
    dest <- fmap ("/" ++) $ defaultParam "redirect" ""
    Just salt <- liftIO $ genSaltUsingPolicy fastBcryptHashingPolicy
    modifyAuthStates $ Map.insert salt $ LT.pack dest
    -- TODO: Fork a process that deletes the auth state after some timeout.
    let query   = [("state", salt)]
    let authUrl  = authorizationUrl key `appendQueryParam` query
        authUrl' = authUrl `appendQueryParam` serviceQuery service
    redirect $ LT.pack $ B.unpack authUrl'

oauthenticate :: ActionOM (OAuth2Result OrionUser)
oauthenticate = do
    srvStr  <- param "service"
    code'   <- param "code"
    state   <- param "state"
    service <- param "service"
    eT      <- getToken service code' state
    eUdat   <- case eT of
                   Left err -> return $ Left err
                   Right t  -> case stringToService srvStr of
                                   Nothing -> return $ Left $ LB.pack $ "Service " ++ srvStr ++ "is not available"
                                   Just s  -> fetchRemoteUserData s t
    let eUT = do t <- eT
                 u <- eUdat
                 return (u,t)
    case eUT of
        Left err    -> return $ Left err
        Right (u,t) -> loginUser service t u

fetchRemoteUserData :: AuthService -> AccessToken -> ActionOM (OAuth2Result UserData)
fetchRemoteUserData service t = do
    mgr <- lift $ asks _oManager
    liftIO $ getUserData mgr service t

loginUser :: AuthService -> AccessToken -> UserData -> ActionOM (OAuth2Result OrionUser)
loginUser service t udat = do
    -- Find any associated accounts.
    accs <- lookupAccounts service udat
    mUser <- case accs of
        -- If there are none, either create a new user or add the account.
        []    -> createNewUserOrAddAccount service t udat
        -- If there is a list of accounts, get the user from that.
        acc:_ -> lookupUserByAccount acc
    return $ case mUser of
        Just user -> Right user
        Nothing   -> Left "Unknown error locating user."

createNewUserOrAddAccount :: AuthService -> AccessToken -> UserData -> ActionOM (Maybe OrionUser)
createNewUserOrAddAccount service t udat = do
    mCookie <- readUserCookie
    case mCookie of
        Nothing -> createNewUser service t udat
        Just (UserCookie uid _) -> addAccountToUser service uid t udat

getToken :: AuthService -> B.ByteString -> B.ByteString -> ActionOM (OAuth2Result AccessToken)
getToken service code state = do
    keys <- lift $ asks _oKeys
    mgr  <- lift $ asks _oManager
    let Just key = oauth2serviceKey keys service
        (url, body') = accessTokenUrl key code
        query        = [("state", state)]
    ebs <- liftIO $ doSimplePostRequest mgr key url (body' ++ query)
    return $ case ebs of
        Left err -> Left err
        Right bs -> decodeToken service bs

decodeToken :: AuthService -> LB.ByteString -> Either LB.ByteString AccessToken
decodeToken service bs =
    let jsn = if service == Facebook then parseQuery bs
              else case decode bs of
                       Just b  -> b
                       Nothing -> object []
    in case fromJSON jsn of
           Error err -> Left $ LB.pack err
           Success t -> Right t

getUserData :: Manager -> AuthService -> AccessToken -> IO (OAuth2Result UserData)
getUserData mgr service token = fetchURLData mgr token (loginUrl service)

fetchURLData :: FromJSON a => Manager -> AccessToken -> URI -> IO (OAuth2Result a)
fetchURLData mgr token uri = do
    ebs <- authGetBS mgr token uri
    return $ case ebs of
        Left l -> Left l
        Right bs -> parseResponse bs

parseResponse :: FromJSON a => LB.ByteString -> OAuth2Result a
parseResponse bs =
    let j  = deunicode bs
    in case decode j of
           Nothing    -> Left $ "Could not decode data: " `LB.append` j
           Just data' -> Right data'

deunicode :: LB.ByteString -> LB.ByteString
deunicode bs =
    case convertUnicode $ T.pack $ LB.unpack bs of
        Left _  -> bs
        Right t -> LB.pack $ T.unpack t

parseQuery :: LB.ByteString -> Value
parseQuery bs = do
    let ps = transpose $ map (T.split (=='=')) $ T.split (=='&') $ T.pack $ LB.unpack bs
    case ps of
        names:values:_ -> object $ zip names $ map toJSON values
        _ -> object []

