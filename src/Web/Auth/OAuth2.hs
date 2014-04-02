{-# LANGUAGE OverloadedStrings #-}
module Web.Auth.OAuth2 where


import           Web.Orion
import           Web.Session
import           Web.Parsers
import           Web.Auth.Service
import           Web.Database.Types
import           Web.Database.Operations
import           Web.Scotty.Trans
import           Web.Auth.Types
import           Network.OAuth.OAuth2
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
    let query   = [("state", salt)]
    let authUrl  = authorizationUrl key `appendQueryParam` query
        authUrl' = authUrl `appendQueryParam` serviceQuery service
    redirect $ LT.pack $ B.unpack authUrl'


oauthenticate :: ActionOM (OAuth2Result OrionUser)
oauthenticate = do
    eUdat   <- fetchRemoteUserData
    service <- param "service"
    case eUdat of
        Left err -> return $ Left err
        Right u  -> loginUser service u


fetchRemoteUserData :: ActionOM (OAuth2Result UserData)
fetchRemoteUserData = do
    service <- param "service"
    code'   <- param "code"
    state   <- param "state"
    let key = serviceKey service
        (url, body') = accessTokenUrl key code'
        query        = [("state", state)]
    ebs <- liftIO $ doSimplePostRequest url $ body' ++ query
    case ebs of
        Left err -> return $ Left err
        Right bs -> parseToken service bs


loginUser :: AuthService -> UserData -> ActionOM (OAuth2Result OrionUser)
loginUser service udat = do
    -- Find any associated accounts.
    accs <- lookupAccounts service udat
    mUser <- case accs of
        -- If there are none, either create a new user or add the account.
        []    -> createNewUserOrAddAccount service udat
        -- If there is a list of accounts, get the user from that.
        acc:_ -> lookupUserByAccount acc
    return $ case mUser of
        Just user -> Right user
        Nothing   -> Left "Unknown error locating user."


createNewUserOrAddAccount :: AuthService -> UserData -> ActionOM (Maybe OrionUser)
createNewUserOrAddAccount service udat = do
    mCookie <- readUserCookie
    case mCookie of
        Nothing -> createNewUser service udat
        Just (UserCookie uid _) -> addAccountToUser service uid udat


parseToken :: AuthService -> LB.ByteString -> ActionOM (OAuth2Result UserData)
parseToken service bs = do
    let jsn = if service == Facebook then parseQuery bs
              else case decode bs of
                       Just b  -> b
                       Nothing -> object []
    case fromJSON jsn of
        Error err -> return $ Left $ LB.pack err
        Success t -> liftIO $ getUserData service t


getUserData :: AuthService -> AccessToken -> IO (OAuth2Result UserData)
getUserData service token = do
    ebs <- authGetBS token $ serviceUrl service
    case ebs of
        Left l -> return $ Left l
        Right bs -> parseUserInfo bs


parseUserInfo :: LB.ByteString -> IO (OAuth2Result UserData)
parseUserInfo bs = do
    let j  = deunicode bs
    case decode j of
        Nothing    -> return $ Left $ "Could not decode user data: " `LB.append` j
        Just data' -> return $ Right data'


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



