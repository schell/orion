{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.User where

import           Web.Scotty.Trans
import           Web.Parsers
import           Network.OAuth.OAuth2
import           Control.Applicative
import           Control.Monad
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as LB
import Debug.Trace


data AuthService = Github | Facebook deriving (Show, Eq)


instance Parsable AuthService where
    parseParam pm =
        case LT.toLower pm of
            "github"   -> Right Github
            "facebook" -> Right Facebook
            _          -> Left "Service is not one of github or facebook."


zeroDay :: UTCTime
zeroDay = UTCTime (ModifiedJulianDay 0) 0


getUserInfo :: AuthService -> AccessToken -> IO (OAuth2Result User)
getUserInfo service token = do
    ebs <- authGetBS token $ serviceUrl service
    case ebs of
        Left l -> return $ Left l
        Right bs -> parseUserInfo service token bs


parseUserInfo :: AuthService -> AccessToken -> LB.ByteString -> IO (OAuth2Result User)
parseUserInfo service token bs = do
    let j  = deunicode bs
    case decode j of
        Nothing    -> return $ Left $ "Could not decode user data: " `LB.append` j
        Just data' -> do t <- fmap (addUTCTime $ 10 * 60) getCurrentTime
                         let creds = UserCreds token t
                         return $ Right $ serviceUser service creds data'


deunicode :: LB.ByteString -> LB.ByteString
deunicode bs =
    case convertUnicode $ T.pack $ LB.unpack bs of
        Left _  -> bs
        Right t -> LB.pack $ T.unpack t


serviceUrl :: AuthService -> URI
serviceUrl Github = "https://api.github.com/user"
serviceUrl Facebook = "https://graph.facebook.com/me?fields=id,name,username,email&"


serviceUser :: AuthService -> UserCreds -> UserData -> User
serviceUser Github = GithubUser
serviceUser Facebook = FacebookUser


instance ToJSON AccessToken where
    toJSON (AccessToken at Nothing) = object [ "access_token" .= B.unpack at ]
    toJSON (AccessToken at (Just rt)) = object [ "access_token" .= B.unpack at
                                               , "refresh_token" .= B.unpack rt
                                               ]


data User = GithubUser { _uCreds :: UserCreds
                       , _uData  :: UserData
                       }
          | FacebookUser { _uCreds :: UserCreds
                         , _uData  :: UserData
                         } deriving (Show)


instance FromJSON User where
    parseJSON v = parseGithubUser v <|> parseFacebookUser v


instance ToJSON User where
    toJSON (GithubUser creds data') = object [ "auth_service" .= ("github" :: String)
                                             , "creds" .= creds
                                             , "data" .= data'
                                             ]
    toJSON (FacebookUser creds data') = object [ "auth_service" .= ("facebook" :: String)
                                               , "creds" .= creds
                                               , "data" .= data'
                                               ]


parseGithubUser :: Value -> Parser User
parseGithubUser (Object o) = do
    "github" <- o .: "auth_service" :: Parser String
    GithubUser <$> o .: "creds"
               <*> o .: "data"
parseGithubUser _ = mzero


parseFacebookUser :: Value -> Parser User
parseFacebookUser (Object o) = do
    "facebook" <- o .: "auth_service" :: Parser String
    FacebookUser <$> o .: "creds"
               <*> o .: "data"
parseFacebookUser _ = mzero


data UserCreds = UserCreds { _ucToken   :: AccessToken
                           , _ucExpires :: UTCTime
                           } deriving (Show)


instance FromJSON UserCreds where
    parseJSON (Object o) = UserCreds
                           <$> o .: "token"
                           <*> o .: "expires"
    parseJSON _ = mzero

instance ToJSON UserCreds where
    toJSON (UserCreds token expires) =
        object [ "token" .= token
               , "expires" .= expires
               ]


data UserData = UserData { _udId      :: Integer
                         , _udLogin   :: Text
                         , _udName    :: Text
                         , _udEmail   :: Text
                         } deriving (Show, Eq)


instance FromJSON UserData where
    parseJSON o = parseFacebookUserData o <|> parseGithubUserData o


parseGithubUserData :: Value -> Parser UserData
parseGithubUserData (Object o) =
    UserData <$> o .: "id"
             <*> o .: "login"
             <*> o .: "name"
             <*> o .: "email"
parseGithubUserData _ = mzero


parseFacebookUserData :: Value -> Parser UserData
parseFacebookUserData (Object o) =
    UserData <$> (read <$> o .: "id")
             <*> o .: "username"
             <*> o .: "name"
             <*> o .: "email"
parseFacebookUserData _ = mzero


instance ToJSON UserData where
    toJSON (UserData id' login name email) =
        object [ "id" .= id'
               , "login" .= login
               , "name" .= name
               , "email" .= email
               ]

